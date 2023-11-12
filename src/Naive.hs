{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Naive (toByteString, fromByteString) where

import ByteString.StrictBuilder (Builder, builderBytes, bytes, word8)
import Control.Applicative ((<*>))
import Control.Category ((.))
import Data.Bits (unsafeShiftL, unsafeShiftR, (.&.))
import Data.Bool (otherwise, (&&))
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
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Maybe (Maybe (Just, Nothing))
import Data.Monoid (mempty, (<>))
import Data.Ord (max, (>=))
import GHC.ByteOrder (ByteOrder (BigEndian, LittleEndian))
import GHC.Err (error)
import GHC.Num (Integer, signum, subtract, (*), (+), (-))
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
  _ -> builderBytes . go mempty 0 $ i
  where
    go :: Builder -> Int -> Integer -> Builder
    go acc written remaining
      | remaining == 0 && written >= requestedLength = acc
      | remaining == 0 =
          let toWrite = requestedLength - written
              padding = replicate toWrite 0x0
           in case requestedByteOrder of
                BigEndian -> bytes padding <> acc
                LittleEndian -> acc <> bytes padding
      | otherwise =
          let newRemaining = remaining `unsafeShiftR` 8
              byteToWrite = fromIntegral (remaining .&. mask)
              newAcc = case requestedByteOrder of
                BigEndian -> word8 byteToWrite <> acc
                LittleEndian -> acc <> word8 byteToWrite
           in go newAcc (written + 1) newRemaining
    mask :: Integer
    !mask = 0xFF

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
  | otherwise = case (,) <$> findIndex (/= 0) bs <*> findIndexEnd (/= 0) bs of
      Nothing -> 0
      Just (firstNZ, lastNZ) ->
        let (firstReadIndex, stoppingPoint, step, initialShift) = case statedByteOrder of
              BigEndian -> (lastNZ, firstNZ - 1, subtract 1, 8 * (length bs - lastNZ - 1))
              LittleEndian -> (firstNZ, lastNZ + 1, (+ 1), 8 * firstNZ)
         in go 0 firstReadIndex stoppingPoint step initialShift
  where
    go :: Integer -> Int -> Int -> (Int -> Int) -> Int -> Integer
    go acc ix limit step shift
      | ix == limit = acc
      | otherwise =
          let newIx = step ix
              newShift = 8 + shift
              newAcc = case index bs ix of
                0x0 -> acc
                w8 -> acc + fromIntegral w8 `unsafeShiftL` shift
           in go newAcc newIx limit step newShift
