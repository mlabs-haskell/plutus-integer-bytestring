{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Naive (toByteString, fromByteString) where

import ByteString.StrictBuilder
  ( Builder,
    builderBytes,
    builderLength,
    bytes,
    storable,
    word64BE,
    word8,
  )
import Control.Category ((.))
import Data.Bits
  ( unsafeShiftL,
    unsafeShiftR,
    (.|.),
  )
import Data.Bool (otherwise)
import Data.ByteString
  ( ByteString,
    findIndex,
    findIndexEnd,
    index,
    length,
    null,
    replicate,
  )
import Data.Eq ((/=), (==))
import Data.Function (($))
import Data.Int (Int)
import Data.Maybe (Maybe (Just, Nothing))
import Data.Monoid (mempty, (<>))
import Data.Ord (max, (<), (<=), (>), (>=))
import Data.Word (Word64, Word8)
import GHC.ByteOrder (ByteOrder (BigEndian, LittleEndian))
import GHC.Err (error)
import GHC.Num
  ( Integer,
    fromInteger,
    signum,
    (+),
    (-),
  )
import GHC.Real (fromIntegral)

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
-- Throughout, @i@ is not negative, and @k@ is positive.
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
toByteString requestedLength requestedByteOrder i = case signum i of
  (-1) -> error "toByteString: negative Integers cannot be converted"
  0 -> replicate (max 1 requestedLength) 0x0
  _ -> builderBytes . go mempty $ i
  where
    -- To improve performance, we extract 8 digits at a time with each 'step' of
    -- the loop. As each 'digit extraction' is a linear-time operation on an
    -- Integer, by doing this, we improve performance by a factor of (roughly)
    -- 8.
    go :: Builder -> Integer -> Builder
    go acc remaining
      | remaining == 0 = padAtEnd acc
      | otherwise =
          let -- Equivalent to remaining `quotRem` (2 ^ 64), but much faster.
              newRemaining = remaining `unsafeShiftR` 64
              digitGroup = fromInteger remaining
           in case newRemaining of
                0 -> finish acc digitGroup
                _ -> go (mkNewAcc64 acc digitGroup) newRemaining
    -- We write this function (and other helpers) in this somewhat convoluted
    -- way to avoid re-scrutinizing requestedByteOrder: if we wrote it the more
    -- sensible way (involving a case statement), we would be introducing a
    -- branch for every time that function would be called. Using this method,
    -- we branch only once (right at the start of the toByteString call).
    padAtEnd :: Builder -> Builder
    !padAtEnd = case requestedByteOrder of
      BigEndian -> \acc ->
        bytes (replicate (requestedLength - builderLength acc) 0x0) <> acc
      LittleEndian -> \acc ->
        acc <> bytes (replicate (requestedLength - builderLength acc) 0x0)
    mkNewAcc64 :: Builder -> Word64 -> Builder
    !mkNewAcc64 = case requestedByteOrder of
      BigEndian -> \acc w64 -> word64BE w64 <> acc
      LittleEndian -> \acc w64 -> acc <> storable w64
    -- When we have 7 or fewer digits remaining, we revert to extracting one
    -- digit at a time. Because a Word64 will fit such a number, and Word64 has
    -- constant-time operations over it, we switch to a Word64 instead of an
    -- Integer for this stage.
    finish :: Builder -> Word64 -> Builder
    finish acc remaining
      | remaining == 0 = padAtEnd acc
      | otherwise =
          let newRemaining = remaining `unsafeShiftR` 8
              digit = fromIntegral remaining
           in finish (mkNewAcc8 acc digit) newRemaining
    mkNewAcc8 :: Builder -> Word8 -> Builder
    !mkNewAcc8 = case requestedByteOrder of
      BigEndian -> \acc w8 -> word8 w8 <> acc
      LittleEndian -> \acc w8 -> acc <> word8 w8

-- | Given a byte order for the representation (as per the Representation
-- section of the 'toByteString' documentation) and a 'ByteString'
-- representation, produce its corresponding 'Integer'.
--
-- = Representation
--
-- If the stated byte order is little-endian, the given representation is as
-- follows:
--
-- 1. The base-256 digits encoding the number, in ascending place value; then
-- 2. Zero or more padding bytes, all of which are zero.
--
-- If the stated byte order is big-endian, the given representation is instead
-- as follows:
--
-- 1. Zero or more padding bytes, all of which are zero; then
-- 2. The base-256 digits encoding the number, in descending place value.
--
-- The only exception to both of these is zero, which is simply a non-zero
-- number of zero bytes.
--
-- = Algorithm
--
-- 1. If given an empty ByteString argument, report an error and stop.
-- 2. Otherwise, for each digit in the ByteString argument, convert that digit
--    to its corresponding place value, then combine all place values.
--
-- = Important note
--
-- Empty 'ByteStrings' cannot be converted and will fail.
--
-- = Properties
--
-- Throughout, @n@ is positive, @i@ is not negative, @b@ is not zero and @bs@ is
-- not empty.
--
-- 1. @fromByteString BigEndian (replicate n b)@ @=@ @fromByteString
--    LittleEndian (replicate n b)@
-- 2. @fromByteString sbo (singleton w8)@ @=@ @fromIntegral w8@
-- 3. @fromByteString bo (toByteString k bo i)@ @=@ @i@
-- 4. @fromByteString LittleEndian (bs <> replicate n 0)@ @=@ @fromByteString
--    LittleEndian bs@
--
-- Properties 3 and 4, together with Property 3 of 'toByteString' imply the
-- following:
--
-- - @fromByteString BigEndian (replicate n 0 <> bs)@ @=@ @fromByteString
--   BigEndian bs@
fromByteString :: ByteOrder -> ByteString -> Integer
fromByteString statedByteOrder bs
  | null bs = error "fromByteString: cannot convert empty ByteString"
  | otherwise = case statedByteOrder of
      -- We use findIndexEnd (and findIndex for the big endian case) to skip
      -- individually decoding padding bytes, as they don't contribute to the
      -- final answer.
      LittleEndian -> case findIndexEnd (/= 0) bs of
        Nothing -> 0
        Just end -> goLE 0 end 0 0
      BigEndian -> case findIndex (/= 0) bs of
        Nothing -> 0
        Just end -> goBE 0 end 0 (length bs - 1)
  where
    -- To speed up decoding, where possible, we process eight digits at once,
    -- both for the big, and little, endian case.
    goLE :: Integer -> Int -> Int -> Int -> Integer
    goLE acc limit shift ix
      | ix <= (limit - 7) =
          let digitGroup = read64LE ix
              newShift = shift + 64
              newIx = ix + 8
           in if digitGroup == 0
                then goLE acc limit newShift newIx
                else -- The accumulator modifier is the same as 'fromIntegral
                -- digitGroup * 2 ^ shift', but much faster. We use this in
                -- several functions: unfortunately, GHC doesn't optimize this
                -- case for Integers (or, seemingly, in general).
                  goLE (acc + fromIntegral digitGroup `unsafeShiftL` shift) limit newShift newIx
      | otherwise = finishLE acc limit shift ix
    goBE :: Integer -> Int -> Int -> Int -> Integer
    goBE acc limit shift ix
      | ix >= (limit + 7) =
          let digitGroup = read64BE ix
              newShift = shift + 64
              newIx = ix - 8
           in if digitGroup == 0
                then goBE acc limit newShift newIx
                else goBE (acc + fromIntegral digitGroup `unsafeShiftL` shift) limit newShift newIx
      | otherwise = finishBE acc limit shift ix
    -- Once we have fewer than 8 digits to process, we slow down and do them one
    -- at a time.
    finishLE :: Integer -> Int -> Int -> Int -> Integer
    finishLE acc limit shift ix
      | ix > limit = acc
      | otherwise =
          let digit = index bs ix
              newShift = shift + 8
              newIx = ix + 1
           in if digit == 0
                then finishLE acc limit newShift newIx
                else finishLE (acc + fromIntegral digit `unsafeShiftL` shift) limit newShift newIx
    finishBE :: Integer -> Int -> Int -> Int -> Integer
    finishBE acc limit shift ix
      | ix < limit = acc
      | otherwise =
          let digit = index bs ix
              newShift = shift + 8
              newIx = ix - 1
           in if digit == 0
                then finishBE acc limit newShift newIx
                else finishBE (acc + fromIntegral digit `unsafeShiftL` shift) limit newShift newIx
    -- Technically, ByteStrings don't (safely) permit reads larger than one
    -- byte. Thus, we use the workaround here, where we construct a Word64 from
    -- pieces.
    --
    -- This still saves us time in the end, because the cost of converting a
    -- Word8 and a Word64 to an Integer (which we need to accumulate) is about
    -- the same, but a Word64 represents eight digits, not one.
    read64LE :: Int -> Word64
    read64LE startIx =
      fromIntegral (index bs startIx)
        .|. (fromIntegral (index bs (startIx + 1)) `unsafeShiftL` 8)
        .|. (fromIntegral (index bs (startIx + 2)) `unsafeShiftL` 16)
        .|. (fromIntegral (index bs (startIx + 3)) `unsafeShiftL` 24)
        .|. (fromIntegral (index bs (startIx + 4)) `unsafeShiftL` 32)
        .|. (fromIntegral (index bs (startIx + 5)) `unsafeShiftL` 40)
        .|. (fromIntegral (index bs (startIx + 6)) `unsafeShiftL` 48)
        .|. (fromIntegral (index bs (startIx + 7)) `unsafeShiftL` 56)
    read64BE :: Int -> Word64
    read64BE endIx =
      fromIntegral (index bs endIx)
        .|. (fromIntegral (index bs (endIx - 1)) `unsafeShiftL` 8)
        .|. (fromIntegral (index bs (endIx - 2)) `unsafeShiftL` 16)
        .|. (fromIntegral (index bs (endIx - 3)) `unsafeShiftL` 24)
        .|. (fromIntegral (index bs (endIx - 4)) `unsafeShiftL` 32)
        .|. (fromIntegral (index bs (endIx - 5)) `unsafeShiftL` 40)
        .|. (fromIntegral (index bs (endIx - 6)) `unsafeShiftL` 48)
        .|. (fromIntegral (index bs (endIx - 7)) `unsafeShiftL` 56)
