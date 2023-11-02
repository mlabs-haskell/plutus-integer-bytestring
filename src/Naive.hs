{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Naive (toByteString) where

import BSUtils (copyBytes, ptrReverse)
import CTConstants (bytesPerWord)
import Control.Applicative (pure, (*>))
import Control.Monad (when)
import Data.Bool (otherwise)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.Eq ((==))
import Data.Function (($))
import Data.Ord (max)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Storable (pokeByteOff)
import GHC.ByteOrder (ByteOrder (BigEndian, LittleEndian))
import GHC.Err (error)
import GHC.Exts
  ( ByteArray#,
    Int (I#),
    Int#,
    Ptr,
    Word#,
    clz#,
    int2Word#,
    isTrue#,
    quotRemInt#,
    quotRemWord#,
    sizeofByteArray#,
    word2Int#,
    wordToWord8#,
    (<#),
    (==#),
  )
import GHC.Num (subtract, (+), (-))
import GHC.Num.BigNat (bigNatSize, bigNatToWord#)
import GHC.Num.Integer (Integer (IN, IP, IS))
import GHC.Num.WordArray (wordArrayLast#)
import GHC.Real (quot)
import GHC.Word (Word8 (W8#))
import System.IO (IO)
import System.IO.Unsafe (unsafeDupablePerformIO)

-- | Given a desired byte length and a desired byte order (see below), as
-- well as a non-negative integer, constructs its byte-based representation.
--
-- = Representation
--
-- We represent a non-negative integer \(i\) in a base-256 representation; the
-- minimum byte length for such a representation is the number of base-256
-- digits that would be required to represent \(i\), which is
--
-- \[
-- \max(1, \ceil(\log_{256}(i)))
-- \]
--
-- If the desired
-- byte length is larger than this, the number will be padded (see below); if
-- the desired byte length is smaller, the minimum byte length will be used
-- instead.
--
-- If a little-endian byte order is requested, the bytes are laid out in
-- ascending order of place value: the first byte is the least-significant
-- digit. Correspondingly, if a big-endian byte order is requested, the
-- bytes are laid out in descending order of place value: the first byte is the
-- most-significant digit. Padding bytes (which are zero bytes) will be put at
-- the end of the `ByteString` if little-endian byte order is requested, and
-- at the beginning of the `ByteString` if big-endian byte order is
-- requested.
--
-- For example, consider the number \(123,456\). If we call 'toByteString'
-- requesting a length of 0 (meaning the minimum will be used) and a
-- little-endian byte order, the result will be @[ 0x80, 0xC2, 0x01 ]@; if instead
-- we requested a big-endian byte order, it would be @[ 0x01, 0xC2, 0x80 ]@. If
-- instead we requested a length of 4, the corresponding little-endian result
-- would be @[ 0x80, 0xC2, 0x01, 0x00 ]@, while the corresponding big-endian
-- result would be @[ 0x00, 0x01, 0xC2, 0x80 ]@.
--
-- = Important note
--
-- Negative `Integer`s cannot be converted and will fail.
--
-- = Properties
--
-- Throughout, @i@ is a \'sensible\' argument (meaning, not negative).
--
-- 1. @fst <$> uncons (toByteString d LittleEndian i)@ @=@ @Just (fromIntegral
--    $ i `rem` 256)@
-- 2. @snd <$> unsnoc (toByteString d BigEndian i)@ @=@ @Just (fromIntegral
--    $ i `rem` 256)@
toByteString :: Int -> ByteOrder -> Integer -> ByteString
toByteString requestedLength requestedByteOrder = \case
  IS i# ->
    if
        | isTrue# (i# <# 0#) -> error "toByteString: cannot convert a negative Integer"
        | isTrue# (i# ==# 0#) -> BS.singleton 0
        | otherwise -> convertSmall requestedLength requestedByteOrder i#
  IP bn# ->
    if bigNatSize bn# == 1
      then convertOneLimb requestedLength requestedByteOrder (bigNatToWord# bn#)
      else convertLarge requestedLength requestedByteOrder bn#
  IN _ -> error "toByteString: cannot convert a negative Integer"

-- Helpers

-- Handle the case where we're dealing with the optimized representaton
-- (meaning, the Integer to be converted is representable in less than the
-- number of bits in a machine word).
--
-- = Preconditions
--
-- 1. @i# > 0@
--
-- = Algorithm
--
-- 1. Determine the minimal length required to represent i#, then compare with
--    requestedLength, taking the maximum.
-- 2. Make a new ByteString of the length determined in step 1, and fill it
--    with zero bytes.
-- 3. Write each base-256 digit of i# into the new ByteString from step 2. If
--    requestedByteOrder is little-endian, write digits in ascending place
--    value; if requestedByteOrder is big-endian, write digits in descending
--    place value.
convertSmall :: Int -> ByteOrder -> Int# -> ByteString
convertSmall requestedLength requestedByteOrder i# =
  let -- Step 1
      --
      -- We make use of the representation of Int# here by noting that, we can
      -- establish the minimal number of bytes required to represent i# by
      -- counting leading zeroes (as these are technically padding to ensure a
      -- fixed width), dividing by 8, then subtracting this number from the
      -- number of bytes needed to represent an Int#. This can be done optimally
      -- by noting that clz# is a machine primitive (single assembly instruction
      -- on all Tier 1 architectures) and that floor division by 8 is a shift by
      -- 3.
      minimalLength = bytesPerWord - (I# (word2Int# (clz# (int2Word# i#))) `quot` 8)
      finalLength = max minimalLength requestedLength
      (start, stop, move) = case requestedByteOrder of
        BigEndian -> (finalLength - 1, finalLength - 1 - minimalLength, subtract 1)
        LittleEndian -> (0, minimalLength, (+ 1))
   in -- Step 2
      --
      -- We make use of unsafeDupablePerformIO here; this is completely safe, as
      -- we only do it to allow mutable construction of a ByteString. Doing so
      -- immutably (by using concatenations, a builder or converting a list)
      -- would be intolerably slow and memory-consuming, especially when padding
      -- bytes are taken into account. This is a common pattern in ByteString
      -- internals.
      unsafeDupablePerformIO $ BSI.create finalLength $ \ptr -> do
        fillBytes ptr 0 finalLength
        go ptr start stop move i#
  where
    -- Step 3
    --
    -- We again make use of the representation of Int#: by use of quotRem# with
    -- a divisor of 256, we extract the least significant base-256 digit, as
    -- well as whatever digits remain. Since this operation is efficient (shift
    -- and mask only), we don't take the penalty for division: by repeating this
    -- enough times, we can extract every significant digit, in ascending order
    -- of place value.
    --
    -- To ensure we do a minimal amount of non-bulk (meaning, 'one byte at a
    -- time') copying, we use the minimal length calculated above as a limit to
    -- how many such writes we perform. As we know that this effectively gives
    -- us the number of significant digits, we don't lose any information, but
    -- potentially save a lot of work, especially if a lot of padding is
    -- required.
    --
    -- We can adjust for big versus little-endianness by changing the starting
    -- point of the write, as well as whether the index argument ascends or
    -- descends. This has the downside of requiring a backwards write in the
    -- case of big-endian byte order, which is cache-unfriendly, but given the
    -- low number of required writes (at most bytesPerWord - 1), this is an
    -- acceptable cost, especially since doing this using a forward write is
    -- much more complex, likely dwarfing any benefit.
    go :: Ptr Word8 -> Int -> Int -> (Int -> Int) -> Int# -> IO ()
    go ptr ix limit move acc#
      | ix == limit = pure ()
      | otherwise =
          let !(# q#, r# #) = quotRemInt# acc# 256#
              byteToWrite = W8# (wordToWord8# (int2Word# r#))
           in pokeByteOff ptr ix byteToWrite *> go ptr (move ix) limit move q#

-- This uses the same approach as convertSmall, but with the added benefit of
-- knowing for a fact that we need at least a whole machine word's worth of
-- bytes. This is because if we needed (potentially) fewer, the Integer would
-- use the optimized representation as an Int#, and if we needed more, we'd have
-- at least one more limb. Thus, instead of having to calculate how many bytes
-- we need minimally, we just assume we need all of them. This also simplifies
-- the write loop, as it can now run for a fixed number of cycles.
convertOneLimb :: Int -> ByteOrder -> Word# -> ByteString
convertOneLimb requestedLength requestedByteOrder w# =
  let finalLength = max bytesPerWord requestedLength
      indexes = case requestedByteOrder of
        BigEndian -> [bytesPerWord - 1, bytesPerWord - 2 .. 0]
        LittleEndian -> [0, 1 .. bytesPerWord - 1]
   in unsafeDupablePerformIO $ BSI.create finalLength $ \ptr -> do
        fillBytes ptr 0 finalLength
        go ptr w# indexes
  where
    go :: Ptr Word8 -> Word# -> [Int] -> IO ()
    go ptr acc# = \case
      [] -> pure ()
      (ix : ixes) ->
        let !(# q#, r# #) = quotRemWord# acc# 256##
            byteToWrite = W8# (wordToWord8# r#)
         in pokeByteOff ptr ix byteToWrite *> go ptr q# ixes

-- Handle the 'large' case, where we know we have at least two limbs in the
-- representation.
--
-- = Preconditions
--
-- 1. @bn#@ has at least two elements
--
-- = Algorithm
--
-- 1. Determine the minimal length required to represent ba#, then compare with
--    requestedLength, taking the maximum.
-- 2. Make a new ByteString of the length determined in step 1, and fill it with
--    zero bytes.
-- 3. Copy the data from ba# into the ByteString constructed in step 2; if
--    requestedByteOrder is little-endian, the bytes are copied in the same
--    order, whereas if requestedByteOrder is big-endian, the bytes are copied
--    in reverse order.
convertLarge :: Int -> ByteOrder -> ByteArray# -> ByteString
convertLarge requestedLength requestedByteOrder ba# =
  let -- Step 1
      --
      -- As the representation of BigNat# (which underlies this case of Integer)
      -- is 'limbed', it must have a length that is an exact multiple of the
      -- machine word size. Furthermore, BigNat#'s documentation
      -- (https://hackage.haskell.org/package/ghc-bignum-1.3/docs/GHC-Num-BigNat.html#t:BigNat-35-)
      -- specifies that the limbs are stored in ascending order of place value
      -- (confusingly described as 'little-endian'), which means that any
      -- padding zero bytes to 'fill out' the representation will be in the last
      -- limb.
      --
      -- We use a similar technique to Step 1 of convertSmall (as it is
      -- basically the same case, modulo type differences), but instead of
      -- subtracting the calculated padding byte count from the size of a
      -- machine word, we subtract it from the overall _byte_ length of ba#. It
      -- is efficient for similar reasons to Step 1 of convertSmall.
      !lastLimb# = wordArrayLast# ba#
      extraTrailingBytes = I# (word2Int# (clz# lastLimb#)) `quot` 8
      baLength = I# (sizeofByteArray# ba#)
      minimalLength = baLength - extraTrailingBytes
      finalLength = max minimalLength requestedLength
   in -- Step 2
      --
      -- The same reasoning for unsafeDupablePerformIO as for Step 2 of
      -- convertSmall applies here.
      unsafeDupablePerformIO $ BSI.create finalLength $ \ptr -> do
        fillBytes ptr 0 finalLength
        -- Step 3
        --
        -- We can do a direct copy in the little-endian case, as we assume we're
        -- on a little-endian machine architecture (and in fact, refuse to
        -- compile if we aren't). In the big-endian case, as long as we start
        -- reading from a significant byte (that is, skipping any padding), we
        -- can produce our desired representation by just copying everything
        -- backwards, as this is exactly the reverse of the representation used
        -- by BigNat# on little-endian architectures.
        --
        -- Because we have mis-matched arguments (ByteArray# as a source, Ptr
        -- Word8 as a destination), and that reverse copying isn't implemented
        -- anyway, we have to resort to the FFI: see the definition of
        -- copyBytes for why.
        copyBytes ptr ba# minimalLength
        when (requestedByteOrder == BigEndian) (ptrReverse ptr finalLength)
