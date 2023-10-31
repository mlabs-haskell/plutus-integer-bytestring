{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Naive (toByteString, fromByteString) where

import CTConstants (bytesPerWord)
import Control.Applicative (pure, (*>))
import Data.Bits (zeroBits)
import Data.Bool (Bool, not, otherwise)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.Eq ((==))
import Data.Function (($))
import Data.Ord (max)
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
    Word8#,
    clz#,
    int2Word#,
    int8ToWord8#,
    intToInt8#,
    isTrue#,
    quotRemInt#,
    word2Int#,
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
      else mkBig requestedLen flippingRequired bn#
  IN _ -> errNegative
  where
    errNegative :: ByteString
    errNegative = error "Cannot convert negative number"
    mkZero :: ByteString
    mkZero = BS.singleton 0
    flippingRequired :: Bool
    flippingRequired = case targetByteOrder of
      BigEndian -> not wantBigEndian
      LittleEndian -> wantBigEndian
    requestedByteOrder :: ByteOrder
    requestedByteOrder = if wantBigEndian then BigEndian else LittleEndian

fromByteString :: Bool -> ByteString -> Integer
fromByteString isBigEndian bs =
  if BS.length bs == 0
    then error "Cannot convert empty ByteString"
    else _

-- Helpers

-- NOTE: Doesn't work because word-based padding aaaaa

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
        BSI.memset ptr zeroBits (fromIntegral requiredLength)
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

mkBig :: Int -> Bool -> ByteArray# -> ByteString
mkBig requestedLen flippingRequired ba# = _
