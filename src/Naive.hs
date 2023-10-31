{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Naive (toByteString, fromByteString) where

import BSUtils (stripEndZeroes, stripStartZeroes)
import CTConstants (bytesPerWord)
import Control.Applicative (pure, (*>))
import Data.Bits (zeroBits)
import Data.Bool (Bool, otherwise)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.Eq ((==))
import Data.Function (($))
import Data.Ord (max, (<), (>=))
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Storable (pokeByteOff)
import GHC.ByteOrder
  ( ByteOrder (BigEndian, LittleEndian),
    targetByteOrder,
  )
import GHC.Err (error)
import GHC.Exts
  ( ByteArray#,
    Int (I#),
    Int#,
    Ptr,
    clz#,
    indexWord8Array#,
    int2Word#,
    int8ToInt#,
    int8ToWord8#,
    intToInt8#,
    isTrue#,
    quotRemInt#,
    sizeofByteArray#,
    word2Int#,
    word8ToInt8#,
    (+#),
    (-#),
    (<#),
    (==#),
  )
import GHC.Num (subtract, (+), (-))
import GHC.Num.BigNat (bigNatIsZero)
import GHC.Num.Integer (Integer (IN, IP, IS))
import GHC.Real (fromIntegral, quot)
import GHC.Word (Word8 (W8#))
import System.IO (IO)
import System.IO.Unsafe (unsafeDupablePerformIO)

toByteString :: Int -> Bool -> Integer -> ByteString
toByteString requestedLen wantBigEndian = \case
  IS i# ->
    if
        | isTrue# (i# <# 0#) -> errNegative
        | isTrue# (i# ==# 0#) -> mkZero
        | otherwise -> mkSmall requestedLen requestedByteOrder i#
  IP bn# ->
    if bigNatIsZero bn#
      then mkZero
      else mkBig requestedLen requestedByteOrder bn#
  IN _ -> errNegative
  where
    errNegative :: ByteString
    errNegative = error "Cannot convert negative number"
    mkZero :: ByteString
    mkZero = BS.singleton 0
    requestedByteOrder :: ByteOrder
    requestedByteOrder = if wantBigEndian then BigEndian else LittleEndian

fromByteString :: Bool -> ByteString -> Integer
fromByteString isBigEndian bs =
  if BS.length bs == 0
    then error "Cannot convert empty ByteString"
    else
      let _stripped =
            if isBigEndian
              then stripEndZeroes bs
              else stripStartZeroes bs
       in error "Not implemented yet!"

-- Helpers

-- When we have only a single limb, the situation is considerably easier: we can
-- deploy numeric operations to 'disassemble' the limb, thus not needing to
-- concern ourselves with the native byte order at all: only the one that was
-- requested. However, as users are allowed to specify any amount of padding
-- they like, we also have to take this into consideration.
--
-- = Algorithm
--
-- The following assumes that our input is not negative or zero: those cases
-- have been caught previously.
--
-- 1. Calculate the number of leading zeroes in the input i#, then divide it by
--    8 (dropping any remainder) to determine 'unnecessary' bytes for the
--    representation.
-- 2. Compare the requested length to the minimum length required, based on the
--    results of step 1 and how many bytes are used per limb. Take the larger
--    option.
-- 3. Determine where we have to start writing from, and how to step:
--      - If we got asked for big-endian, this will be at the _end_ of the
--      ByteString, and we will be stepping _backwards_.
--      - If we got asked for little-endian, this will be at the _beginning_ of
--      the ByteString, and we will be stepping forwards.
-- 4. Construct a new ByteString of the required length as determined in step 2,
--    and fill it with zeroes to ensure it's properly initialized everywhere.
-- 5. Disassemble i# using the recursive helper, writing the results into the
--    ByteString as we go.
mkSmall :: Int -> ByteOrder -> Int# -> ByteString
mkSmall requestedLen requiredByteOrder i# =
  let -- Step 1
      leadingZeroBits :: Int = I# (word2Int# (clz# (int2Word# i#)))
      leadingZeroBytes = leadingZeroBits `quot` 8
      -- Step 2
      bytesNeeded = bytesPerWord - leadingZeroBytes
      requiredLength = max requestedLen bytesNeeded
      -- Step 3 (plus 'move' helper)
      startingPosition = case requiredByteOrder of
        BigEndian -> requiredLength - 1
        LittleEndian -> 0
   in -- Step 4
      unsafeDupablePerformIO $ BSI.create requiredLength $ \ptr -> do
        fillBytes ptr zeroBits (fromIntegral requiredLength)
        -- Step 5
        go ptr startingPosition i#
  where
    -- = Algorithm
    --
    -- 1. If the accumulator has reached 0, stop.
    -- 2. Otherwise, divmod by 256 to extract the next significant byte.
    -- 3. Write the quotient from 2 to the position given by our index in the
    --    ByteString pointer
    -- 4. Use the step helper determined previously to adjust the write
    --    position, then recurse using the remainder from step 2 as the new
    --    accumulator.
    go :: Ptr Word8 -> Int -> Int# -> IO ()
    go ptr i acc#
      -- Step 1
      | isTrue# (acc# ==# 0#) = pure ()
      -- Step 2
      | otherwise = case quotRemInt# acc# 256# of
          (# q#, r# #) ->
            let toWrite :: Word8 = W8# (int8ToWord8# (intToInt8# q#))
             in -- Steps 3 and 4
                pokeByteOff ptr i toWrite *> go ptr (move i) r#
    move :: Int -> Int
    move = case requiredByteOrder of
      BigEndian -> subtract 1
      LittleEndian -> (+ 1)

-- When we have multiple limbs, there's some added difficulties. The underlying
-- implementation of Integer happens to be a ByteArray#, storing words:
-- effectively, we have a base 2^k representation, where k is the bit size of
-- the native machine word.
--
-- Here, we have to be careful, since there are technically _two_ ordering
-- issues at play:
--
-- - Endianness of the words themselves, which comes from the platform; and
-- - The order of the words-as-digits: either in ascending or descending place
--   value.
--
-- According to the documentation
-- (https://hackage.haskell.org/package/ghc-bignum-1.3/docs/GHC-Num-BigNat.html#t:BigNat-35-),
-- the order of words-as-digits is in ascending order of place value
-- (confusingly labelled 'little-endian' in the description). Thus, if we assume
-- a number comprising of n limbs (l_1, l_2, ..., l_n), each consisting of k
-- bytes (b_1, b_2, ..., b_k), with b_1 being the least significant byte, we
-- have two possible byte layouts. On a little-endian system, this would be:
--
-- (l_1, b_1), (l_1, b_2), ... , (l_1, b_k), (l_2, b_1), ... , (l_n, b_k)
--
-- On a big-endian system, this would be:
--
-- (l_1, b_k), (l_1, b_{k-1}), ... , (l_1, b_1), (l_2, b_k), (l_2, b_{k-1}), ...
-- , (l_n, b_1)
--
-- As 'limbing' is an implementation detail (and is platform-dependent anyway),
-- we choose a different translation. If the requested endianness is
-- little-endian, we lay out the resulting ByteString as follows:
--
-- (l_1, b_1), (l_1, b_2), ... , (l_1, b_k), (l_2, b_1), ... , (l_n, b_k)
--
-- This matches the layout on a little-endian system exactly. If the requested
-- endianness is big-endian, we instead lay out the resulting ByteString as
-- follows:
--
-- (l_n, b_k), (l_n, b_{k-1}), ... , (l_n, b_1), (l_{n-1}, b_k), (l_{n-1},
-- b_{k-1}), ..., (l_1, b_1)
--
-- This is the exact reverse of the little-endian layout described above. This
-- has two advantages: it is independent of any 'limbing' choices, and is easy
-- to grasp. Effectively, we choose to have a representation in base-256, with
-- either ascending or descending place value.
--
-- For efficiency, we allow users to request representation lengths that are not
-- exact multiples of the word size, assuming this would not change the number
-- being represented. In such a case, either trailing (in the little-endian
-- case) or leading (in the big-endian case) zeroes can be dropped.
--
-- = Algorithm
--
-- 1. Determine if there are any non-significant zero bytes in the input
--    ByteArray#. Zero bytes are non-significant if they are in the
--    highest-index limb, and have higher byte positions in that limb than any
--    non-zero byte. Subtract this from the length of the input ByteArray# to
--    determine a minimum representation length.
-- 2. Compare the result from 1 to the requested length; take the maximum.
-- 3. Copy over the bytes in the requested order.
mkBig :: Int -> ByteOrder -> ByteArray# -> ByteString
mkBig requestedLen requestedByteOrder ba# =
  let -- Step 1
      minimalLen = I# (sizeofByteArray# ba# -# unnecessaryZeroes)
      -- Step 2
      requiredLength = max minimalLen requestedLen
   in unsafeDupablePerformIO $ BSI.create requiredLength $ \ptr -> do
        -- Step 3
        case (requestedByteOrder, targetByteOrder) of
          (LittleEndian, LittleEndian) -> copyBAToPtr ba# ptr requiredLength
          (BigEndian, LittleEndian) -> copyBAToPtrReverse ba# ptr requiredLength
          (LittleEndian, BigEndian) -> error "Not implemented yet!"
          (BigEndian, BigEndian) -> error "Not implemented yet!"
  where
    unnecessaryZeroes :: Int#
    unnecessaryZeroes = case targetByteOrder of
      LittleEndian -> countUnnecessaryZeroesLE ba#
      BigEndian -> countUnnecessaryZeroesBE ba#

-- As the layout is as follows:
--
-- (l_1, b_1), (l_1, b_2), ... , (l_1, b_k), (l_2, b_1), ... , (l_n, b_k)
--
-- we only need to count zero bytes from the end until we reach any non-zero.
-- Due to the call source, we know that the input is non-empty.
countUnnecessaryZeroesLE :: ByteArray# -> Int#
countUnnecessaryZeroesLE ba# = go 0# (sizeofByteArray# ba# -# 1#)
  where
    go :: Int# -> Int# -> Int#
    go acc# ix#
      | isTrue# (ix# <# 0#) = acc#
      | otherwise =
          let !read# = int8ToInt# (word8ToInt8# (indexWord8Array# ba# ix#))
           in if isTrue# (read# ==# 0#)
                then go (acc# +# 1#) (ix# -# 1#)
                else acc#

copyBAToPtr :: ByteArray# -> Ptr Word8 -> Int -> IO ()
copyBAToPtr ba# ptr limit = go 0
  where
    go :: Int -> IO ()
    go ix@(I# ix#)
      | ix >= limit = pure ()
      | otherwise =
          let fromBA = W8# (indexWord8Array# ba# ix#)
           in pokeByteOff ptr ix fromBA *> go (ix + 1)

copyBAToPtrReverse :: ByteArray# -> Ptr Word8 -> Int -> IO ()
copyBAToPtrReverse ba# ptr limit = go (limit - 1)
  where
    go :: Int -> IO ()
    go ix@(I# ix#)
      | ix < 0 = pure ()
      | otherwise =
          let fromBA = W8# (indexWord8Array# ba# ix#)
           in pokeByteOff ptr (limit - ix - 1) fromBA *> go (ix - 1)

countUnnecessaryZeroesBE :: ByteArray# -> Int#
countUnnecessaryZeroesBE _ = error "Not implemented yet!"
