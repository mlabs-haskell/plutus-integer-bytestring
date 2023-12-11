{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module WithBinary (toByteString, fromByteString) where

import Control.Applicative (pure)
import Control.Category ((.))
import Data.Binary.Get
  ( Get,
    bytesRead,
    getWord64be,
    getWord64le,
    getWord8,
    isEmpty,
    runGet,
  )
import Data.Binary.Put
  ( Put,
    putByteString,
    putWord64be,
    putWord64le,
    putWord8,
    runPut,
  )
import Data.Bits (unsafeShiftL, unsafeShiftR)
import Data.Bool (Bool (False, True), otherwise)
import Data.ByteString (ByteString, length, null, replicate)
import Data.ByteString.Lazy qualified as Lazy
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Monoid (mempty, (<>))
import Data.Ord (max, (<=), (>), (>=))
import Data.Word (Word64)
import GHC.ByteOrder (ByteOrder (BigEndian, LittleEndian))
import GHC.Err (error)
import GHC.Num (Integer, fromInteger, signum, (*), (+), (-))
import GHC.Real (fromIntegral)
import Text.Show (show)

toByteString :: Int -> ByteOrder -> Integer -> ByteString
toByteString requestedLength requestedByteOrder i = case signum i of
  (-1) -> error "toByteString: negative Integers cannot be converted"
  0 -> replicate (max 1 requestedLength) 0x00
  _ -> Lazy.toStrict . runPut $ case (requestedByteOrder, requestedLength > 0) of
    (LittleEndian, True) -> goLELimit mempty 0 i
    (LittleEndian, False) -> goLENoLimit mempty i
    (BigEndian, True) -> goBELimit mempty 0 i
    (BigEndian, False) -> goBENoLimit mempty i
  where
    goLELimit :: Put -> Int -> Integer -> Put
    goLELimit acc written remaining
      | remaining == 0 = padLE acc written
      | written >= requestedLength = errorOut
      | otherwise =
          let newRemaining = remaining `unsafeShiftR` 64
              digitGroup = fromInteger remaining
           in case newRemaining of
                0 -> finishLELimit acc written digitGroup
                _ -> goLELimit (acc <> putWord64le digitGroup) (written + 8) newRemaining
    finishLELimit :: Put -> Int -> Word64 -> Put
    finishLELimit acc written remaining
      | remaining == 0 = padLE acc written
      | written > requestedLength = errorOut
      | otherwise =
          let newRemaining = remaining `unsafeShiftR` 8
              digit = fromIntegral remaining
           in finishLELimit (acc <> putWord8 digit) (written + 1) newRemaining
    goLENoLimit :: Put -> Integer -> Put
    goLENoLimit acc remaining
      | remaining == 0 = acc
      | otherwise =
          let newRemaining = remaining `unsafeShiftR` 64
              digitGroup = fromInteger remaining
           in case newRemaining of
                0 -> finishLENoLimit acc digitGroup
                _ -> goLENoLimit (acc <> putWord64le digitGroup) newRemaining
    finishLENoLimit :: Put -> Word64 -> Put
    finishLENoLimit acc remaining
      | remaining == 0 = acc
      | otherwise =
          let newRemaining = remaining `unsafeShiftR` 8
              digit = fromIntegral remaining
           in finishLENoLimit (acc <> putWord8 digit) newRemaining
    padLE :: Put -> Int -> Put
    padLE acc written = acc <> putByteString (replicate (requestedLength - written) 0x00)
    goBELimit :: Put -> Int -> Integer -> Put
    goBELimit acc written remaining
      | remaining == 0 = padBE acc written
      | written >= requestedLength = errorOut
      | otherwise =
          let newRemaining = remaining `unsafeShiftR` 64
              digitGroup = fromInteger remaining
           in case newRemaining of
                0 -> finishBELimit acc written digitGroup
                _ -> goBELimit (putWord64be digitGroup <> acc) (written + 8) newRemaining
    finishBELimit :: Put -> Int -> Word64 -> Put
    finishBELimit acc written remaining
      | remaining == 0 = padBE acc written
      | written > requestedLength = errorOut
      | otherwise =
          let newRemaining = remaining `unsafeShiftR` 8
              digit = fromIntegral remaining
           in finishBELimit (putWord8 digit <> acc) (written + 1) newRemaining
    goBENoLimit :: Put -> Integer -> Put
    goBENoLimit acc remaining
      | remaining == 0 = acc
      | otherwise =
          let newRemaining = remaining `unsafeShiftR` 64
              digitGroup = fromInteger remaining
           in case newRemaining of
                0 -> finishBENoLimit acc digitGroup
                _ -> goBENoLimit (putWord64be digitGroup <> acc) newRemaining
    finishBENoLimit :: Put -> Word64 -> Put
    finishBENoLimit acc remaining
      | remaining == 0 = acc
      | otherwise =
          let newRemaining = remaining `unsafeShiftR` 8
              digit = fromIntegral remaining
           in finishBENoLimit (putWord8 digit <> acc) newRemaining
    padBE :: Put -> Int -> Put
    padBE acc written = putByteString (replicate (requestedLength - written) 0x00) <> acc
    errorOut :: Put
    errorOut =
      error $
        "toByteString: cannot represent "
          <> show i
          <> " using "
          <> show requestedLength
          <> " bytes"

fromByteString :: ByteOrder -> ByteString -> Integer
fromByteString statedByteOrder bs
  | null bs = error "fromByteString: cannot convert empty ByteString"
  | otherwise =
      runGet
        ( case statedByteOrder of
            LittleEndian -> goLE 0 0
            BigEndian -> goBE 0
        )
        . Lazy.fromStrict
        $ bs
  where
    len :: Int
    len = length bs
    limit :: Int
    limit = len - 8
    goLE :: Integer -> Int -> Get Integer
    goLE acc shift = do
      done <- isEmpty
      if done
        then pure acc
        else do
          read :: Int <- fromIntegral <$> bytesRead
          if read <= limit
            then do
              digitGroup <- getWord64le
              let newShift = shift + 64
              if digitGroup == 0
                then goLE acc newShift
                else goLE (acc + fromIntegral digitGroup `unsafeShiftL` shift) newShift
            else finishLE acc shift
    finishLE :: Integer -> Int -> Get Integer
    finishLE acc shift = do
      done <- isEmpty
      if done
        then pure acc
        else do
          digit <- getWord8
          let newShift = shift + 8
          if digit == 0
            then finishLE acc newShift
            else finishLE (acc + fromIntegral digit `unsafeShiftL` shift) newShift
    goBE :: Integer -> Get Integer
    goBE acc = do
      read :: Int <- fromIntegral <$> bytesRead
      case len - read of
        0 -> pure acc
        remaining ->
          if remaining <= limit
            then do
              let shift = 8 * (remaining - 1)
              digitGroup <- getWord64be
              if digitGroup == 0
                then goBE acc
                else goBE (acc + fromIntegral digitGroup `unsafeShiftL` shift)
            else finishBE acc
    finishBE :: Integer -> Get Integer
    finishBE acc = do
      read :: Int <- fromIntegral <$> bytesRead
      case len - read of
        0 -> pure acc
        remaining -> do
          let shift = 8 * (remaining - 1)
          digit <- getWord8
          if digit == 0
            then finishBE acc
            else finishBE (acc + fromIntegral digit `unsafeShiftL` shift)
