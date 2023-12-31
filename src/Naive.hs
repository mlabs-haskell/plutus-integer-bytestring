{-# LANGUAGE ImportQualifiedPost #-}
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
import Data.Bits
  ( unsafeShiftL,
    unsafeShiftR,
    (.|.),
  )
import Data.Bool (otherwise)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Eq ((/=), (==))
import Data.Function (($))
import Data.Int (Int)
import Data.Maybe (Maybe (Just, Nothing))
import Data.Monoid (mempty, (<>))
import Data.Ord ((<), (<=), (>), (>=))
import Data.Word (Word64)
import GHC.ByteOrder (ByteOrder (BigEndian, LittleEndian))
import GHC.Err (error)
import GHC.Num
  ( Integer,
    fromInteger,
    (+),
    (-),
  )
import GHC.Real (fromIntegral)
import Text.Show (show)

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
-- \max(1, \lceil \log_{256}(i) \rceil)
-- \]
--
-- If the desired
-- byte length is larger than this, the number will be padded (see below).
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
-- Negative 'Integer's cannot be converted and will fail.
--
-- When the length argument is @0@, we assume that the user wants the
-- minimum-sized representation of the input 'Integer' (that is, no specific
-- size requirements). If the length argument is positive, we instead assume
-- that this is a hard requirement: if we would require more digits than this,
-- we error, and if we would require fewer, we pad.
--
-- = Properties
--
-- Throughout, @i@ is not negative.
--
-- 1. @fst <$> uncons (toByteString 0 LittleEndian i)@ @=@ @Just (fromIntegral
--    $ i `rem` 256)@
-- 2. @toByteString d LittleEndian (fromIntegral w8)@ @=@ @cons w8 (replicate
--    (max 0 (d - 1) 0))@
-- 3. @toByteString 0 LittleEndian i@ @=@ @reverse (toByteString 0 BigEndian i)@
--
-- Properties 1 and 3 together imply the following:
--
-- - @snd <$> unsnoc (toByteString 0 BigEndian i)@ @=@ @Just (fromIntegral
--   $ i `rem` 256)@
toByteString :: Int -> ByteOrder -> Integer -> ByteString
toByteString requestedLength requestedByteOrder i
  | i < 0 = error "toByteString: negative Integers cannot be converted"
  | requestedLength < 0 = error "toByteString: negative lengths don't make sense"
  | i == 0 = BS.replicate requestedLength 0x00
  | requestedLength == 0 = builderBytes $ case requestedByteOrder of
      LittleEndian -> goLENoLimit mempty i
      BigEndian -> goBENoLimit mempty i
  | otherwise = builderBytes $ case requestedByteOrder of
      LittleEndian -> goLELimit mempty i
      BigEndian -> goBELimit mempty i
  where
    -- To improve performance, we extract 8 digits at a time with each 'step' of
    -- the loop. As each 'digit extraction' is a linear-time operation on an
    -- Integer, by doing this, we improve performance by a factor of (roughly)
    -- 8.
    goLELimit :: Builder -> Integer -> Builder
    goLELimit acc remaining
      | remaining == 0 = padLE acc
      | builderLength acc >= requestedLength = errorOut
      | otherwise =
          -- The same as x `quotRem` 256^8, but much faster.
          let newRemaining = remaining `unsafeShiftR` 64
              digitGroup = fromInteger remaining
           in case newRemaining of
                0 -> finishLELimit acc digitGroup
                _ -> goLELimit (acc <> storable digitGroup) newRemaining
    goBELimit :: Builder -> Integer -> Builder
    goBELimit acc remaining
      | remaining == 0 = padBE acc
      | builderLength acc >= requestedLength = errorOut
      | otherwise =
          let newRemaining = remaining `unsafeShiftR` 64
              digitGroup = fromInteger remaining
           in case newRemaining of
                0 -> finishBELimit acc digitGroup
                _ -> goBELimit (word64BE digitGroup <> acc) newRemaining
    goLENoLimit :: Builder -> Integer -> Builder
    goLENoLimit acc remaining
      | remaining == 0 = acc
      | otherwise =
          let newRemaining = remaining `unsafeShiftR` 64
              digitGroup = fromInteger remaining
           in case newRemaining of
                0 -> finishLENoLimit acc digitGroup
                _ -> goLENoLimit (acc <> storable digitGroup) newRemaining
    goBENoLimit :: Builder -> Integer -> Builder
    goBENoLimit acc remaining
      | remaining == 0 = acc
      | otherwise =
          let newRemaining = remaining `unsafeShiftR` 64
              digitGroup = fromInteger remaining
           in case newRemaining of
                0 -> finishBENoLimit acc digitGroup
                _ -> goBENoLimit (word64BE digitGroup <> acc) newRemaining
    -- When we have 7 or fewer digits remaining, we revert to extracting one
    -- digit at a time. Because a Word64 will fit such a number, and Word64 has
    -- constant-time operations over it, we switch to a Word64 instead of an
    -- Integer for this stage.
    finishLELimit :: Builder -> Word64 -> Builder
    finishLELimit acc remaining
      | remaining == 0 = padLE acc
      | builderLength acc > requestedLength = errorOut
      | otherwise =
          -- The same as w64 `quotRem` 256, but much faster.
          let newRemaining = remaining `unsafeShiftR` 8
              digit = fromIntegral remaining
           in finishLELimit (acc <> word8 digit) newRemaining
    finishLENoLimit :: Builder -> Word64 -> Builder
    finishLENoLimit acc remaining
      | remaining == 0 = acc
      | otherwise =
          let newRemaining = remaining `unsafeShiftR` 8
              digit = fromIntegral remaining
           in finishLENoLimit (acc <> word8 digit) newRemaining
    finishBELimit :: Builder -> Word64 -> Builder
    finishBELimit acc remaining
      | remaining == 0 = padBE acc
      | builderLength acc > requestedLength = errorOut
      | otherwise =
          let newRemaining = remaining `unsafeShiftR` 8
              digit = fromIntegral remaining
           in finishBELimit (word8 digit <> acc) newRemaining
    finishBENoLimit :: Builder -> Word64 -> Builder
    finishBENoLimit acc remaining
      | remaining == 0 = acc
      | otherwise =
          let newRemaining = remaining `unsafeShiftR` 8
              digit = fromIntegral remaining
           in finishBENoLimit (word8 digit <> acc) newRemaining
    errorOut :: Builder
    errorOut =
      error $
        "toByteString: cannot represent "
          <> show i
          <> " using "
          <> show requestedLength
          <> " bytes"
    padLE :: Builder -> Builder
    padLE acc = acc <> bytes (BS.replicate (requestedLength - builderLength acc) 0x0)
    padBE :: Builder -> Builder
    padBE acc = bytes (BS.replicate (requestedLength - builderLength acc) 0x0) <> acc

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
-- = Properties
--
-- Throughout, @n@ is positive, @i@ is not negative, @b@ is not zero and @bs@ is
-- not empty.
--
-- 1. @fromByteString BigEndian (replicate n b)@ @=@ @fromByteString
--    LittleEndian (replicate n b)@
-- 2. @fromByteString sbo (singleton w8)@ @=@ @fromIntegral w8@
-- 3. @fromByteString bo (toByteString 0 bo i)@ @=@ @i@
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
  | BS.null bs = 0
  | otherwise = case statedByteOrder of
      -- We use findIndexEnd (and findIndex for the big endian case) to skip
      -- individually decoding padding bytes, as they don't contribute to the
      -- final answer.
      LittleEndian -> case BS.findIndexEnd (/= 0) bs of
        Nothing -> 0
        Just end -> goLE 0 end 0
      BigEndian -> case BS.findIndex (/= 0) bs of
        Nothing -> 0
        Just end -> goBE 0 end 0 (BS.length bs - 1)
  where
    -- To speed up decoding, where possible, we process eight digits at once,
    -- both for the big, and little, endian case.
    goLE :: Integer -> Int -> Int -> Integer
    goLE acc limit ix
      | ix <= (limit - 7) =
          let digitGroup = read64LE ix
              -- Same as ix * 8, but faster. GHC might optimize this already,
              -- but we may as well be sure.
              shift = ix `unsafeShiftL` 3
              newIx = ix + 8
           in -- The accumulator modifier is the same as 'fromIntegral
              -- digitGroup * 2 ^ shift', but much faster. We use this in
              -- several functions: unfortunately, GHC doesn't optimize this
              -- case for Integers (or, seemingly, in general).
              goLE (acc + fromIntegral digitGroup `unsafeShiftL` shift) limit newIx
      | otherwise = finishLE acc limit ix
    -- Because shifts and indexes change in different ways here (indexes start
    -- high and lower, while shifts start low and rise), we track them
    -- separately. It doesn't really change anything, as in order to calculate
    -- them in-situ (like we do for the little endian case), we would have to
    -- pass a length argument anyway.
    goBE :: Integer -> Int -> Int -> Int -> Integer
    goBE acc limit shift ix
      | ix >= (limit + 7) =
          let digitGroup = read64BE ix
              newShift = shift + 64
              newIx = ix - 8
           in goBE (acc + fromIntegral digitGroup `unsafeShiftL` shift) limit newShift newIx
      | otherwise = finishBE acc limit shift ix
    -- Once we have fewer than 8 digits to process, we slow down and do them one
    -- at a time.
    finishLE :: Integer -> Int -> Int -> Integer
    finishLE acc limit ix
      | ix > limit = acc
      | otherwise =
          let digit = BS.index bs ix
              shift = ix `unsafeShiftL` 3
              newIx = ix + 1
           in finishLE (acc + fromIntegral digit `unsafeShiftL` shift) limit newIx
    finishBE :: Integer -> Int -> Int -> Int -> Integer
    finishBE acc limit shift ix
      | ix < limit = acc
      | otherwise =
          let digit = BS.index bs ix
              newShift = shift + 8
              newIx = ix - 1
           in finishBE (acc + fromIntegral digit `unsafeShiftL` shift) limit newShift newIx
    -- Technically, ByteStrings don't (safely) permit reads larger than one
    -- byte. Thus, we use the workaround here, where we construct a Word64 from
    -- pieces.
    --
    -- This still saves us time in the end, because the cost of converting a
    -- Word8 and a Word64 to an Integer (which we need to accumulate) is about
    -- the same, but a Word64 represents eight digits, not one.
    read64LE :: Int -> Word64
    read64LE startIx =
      fromIntegral (BS.index bs startIx)
        .|. (fromIntegral (BS.index bs (startIx + 1)) `unsafeShiftL` 8)
        .|. (fromIntegral (BS.index bs (startIx + 2)) `unsafeShiftL` 16)
        .|. (fromIntegral (BS.index bs (startIx + 3)) `unsafeShiftL` 24)
        .|. (fromIntegral (BS.index bs (startIx + 4)) `unsafeShiftL` 32)
        .|. (fromIntegral (BS.index bs (startIx + 5)) `unsafeShiftL` 40)
        .|. (fromIntegral (BS.index bs (startIx + 6)) `unsafeShiftL` 48)
        .|. (fromIntegral (BS.index bs (startIx + 7)) `unsafeShiftL` 56)
    read64BE :: Int -> Word64
    read64BE endIx =
      fromIntegral (BS.index bs endIx)
        .|. (fromIntegral (BS.index bs (endIx - 1)) `unsafeShiftL` 8)
        .|. (fromIntegral (BS.index bs (endIx - 2)) `unsafeShiftL` 16)
        .|. (fromIntegral (BS.index bs (endIx - 3)) `unsafeShiftL` 24)
        .|. (fromIntegral (BS.index bs (endIx - 4)) `unsafeShiftL` 32)
        .|. (fromIntegral (BS.index bs (endIx - 5)) `unsafeShiftL` 40)
        .|. (fromIntegral (BS.index bs (endIx - 6)) `unsafeShiftL` 48)
        .|. (fromIntegral (BS.index bs (endIx - 7)) `unsafeShiftL` 56)
