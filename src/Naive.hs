{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Naive (toByteString) where

import BSUtils (copyBytes, copyBytesReverse)
import CTConstants (bytesPerWord)
import Control.Applicative (pure, (*>))
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
    clz#,
    int2Word#,
    isTrue#,
    quotRemInt#,
    sizeofByteArray#,
    word2Int#,
    wordToWord8#,
    (<#),
    (==#),
  )
import GHC.Num (subtract, (+), (-))
import GHC.Num.Integer (Integer (IN, IP, IS))
import GHC.Num.WordArray (wordArrayLast#)
import GHC.Real (quot)
import GHC.Word (Word8 (W8#))
import System.IO (IO)

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
-- Throughout, @i@ is a \'sensible\' argument (meaning, not negative), and @k@
-- is positive.
--
-- 1. @fst <$> uncons (toByteString d LittleEndian i)@ @=@ @Just (fromIntegral
--    $ i `rem` 256)@
-- 2. @toByteString d LittleEndian (fromIntegral w8)@ @=@ @cons w8 (replicate
--    (max 0 (d - 1) 0))@
-- 3. @toByteString d LittleEndian i@ @=@ @reverse (toByteString d BigEndian i)@
--
-- Properties 1 and 3 together imply the following:
--
-- - @snd <$> unsnoc (toByteString d BigEndian i)@ @=@ @Just (fromIntegral
--   $ i `rem` 256)@
toByteString :: Int -> ByteOrder -> Integer -> ByteString
toByteString requestedLength requestedByteOrder = \case
  IS i# ->
    if
        | isTrue# (i# <# 0#) -> error "toByteString: cannot convert a negative Integer"
        | isTrue# (i# ==# 0#) -> BS.replicate (max 1 requestedLength) 0
        | otherwise -> convertSmall requestedLength requestedByteOrder i#
  IP bn# -> convertLarge requestedLength requestedByteOrder bn#
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
      -- unsafeCreate allows us to mutably construct the result. While it looks
      -- scary, it's completely safe here, as we don't leak the internal Ptr to
      -- the outside world. Making this string immutably (by using
      -- concatenations, a builder or converting a list) would be intolerably
      -- slow and memory-consuming, especially when we take padding bytes into
      -- account. This kind of construction is also a common pattern in the
      -- bytestring library itself.
      BSI.unsafeCreate finalLength $ \ptr -> do
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

-- Handle the 'large' case, where we know we have at least one full limb in the
-- representation.
--
-- = Preconditions
--
-- 1. @bn#@ has at least one element
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
      -- The same reasoning for unsafeCreate as for Step 2 of convertSmall applies here.
      BSI.unsafeCreate finalLength $ \ptr -> do
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
        case requestedByteOrder of
          LittleEndian -> copyBytes ptr ba# minimalLength
          BigEndian -> copyBytesReverse ptr ba# minimalLength finalLength
