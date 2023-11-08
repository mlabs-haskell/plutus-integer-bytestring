{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Optimized (fromByteString) where

import BSUtils (copyBytes', copyBytesReverse')
import CTConstants (bytesPerWord)
import Control.Applicative (pure)
import Control.Category ((.))
import Control.Monad (foldM, (>>=))
import Data.Bits (unsafeShiftL)
import Data.Bool (otherwise)
import Data.ByteString
  ( ByteString,
    findIndex,
    findIndexEnd,
    length,
    null,
    useAsCStringLen,
  )
import Data.Eq ((/=), (==))
import Data.Function (($))
import Data.Functor ((<&>))
import Data.Int (Int)
import Data.Maybe (fromMaybe)
import Data.Ord (Ordering (EQ, GT, LT), compare)
import Data.Primitive.ByteArray
  ( ByteArray (ByteArray),
    MutableByteArray (MutableByteArray),
    fillByteArray,
    getSizeofMutableByteArray,
    newByteArray,
    unsafeFreezeByteArray,
  )
import Data.Word (Word, Word16, Word8, byteSwap16)
import Foreign.C.Types (CChar)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (peek, peekByteOff)
import GHC.ByteOrder (ByteOrder (BigEndian, LittleEndian))
import GHC.Err (error)
import GHC.Exts (byteSwap64#)
import GHC.Num ((*), (+), (-))
import GHC.Num.Integer (Integer (IP), integerFromWord, integerFromWord64#)
import GHC.Real (even, fromIntegral, quotRem)
import GHC.Word (Word64 (W64#))
import System.IO (IO)
import System.IO.Unsafe (unsafeDupablePerformIO)

fromByteString :: ByteOrder -> ByteString -> Integer
fromByteString statedByteOrder bs
  | null bs = error "fromByteString: cannot convert empty ByteString"
  | otherwise =
      let len = length bs
       in case statedByteOrder of
            LittleEndian ->
              let msIndex = fromMaybe (len - 1) (findIndexEnd (/= 0) bs)
                  copySpan = msIndex + 1
               in case compare copySpan bytesPerWord of
                    LT -> mkSmallLE copySpan bs
                    EQ -> mkSingleLE bs
                    GT -> mkBigLE copySpan bs
            BigEndian ->
              let msIndex = fromMaybe 0 (findIndex (/= 0) bs)
                  copySpan = len - msIndex
               in case compare copySpan 8 of
                    LT -> mkSmallBE msIndex copySpan bs
                    EQ -> mkSingleBE msIndex bs
                    GT -> mkBigBE msIndex copySpan bs

-- Helpers

mkSingleLE :: ByteString -> Integer
mkSingleLE bs = unsafeDupablePerformIO . useAsCStringLen bs $ \(ptr, _) -> do
  let bigPtr :: Ptr Word = castPtr ptr
  integerData <- peek bigPtr
  pure . integerFromWord $ integerData

mkSingleBE :: Int -> ByteString -> Integer
mkSingleBE msIndex bs = unsafeDupablePerformIO . useAsCStringLen bs $ \(ptr, _) -> do
  (W64# integerDataBackwards#) <- peekByteOff ptr msIndex
  let integerData# = byteSwap64# integerDataBackwards#
  pure (integerFromWord64# integerData#)

mkSmallLE :: Int -> ByteString -> Integer
mkSmallLE span bs = unsafeDupablePerformIO . useAsCStringLen bs $ \(ptr, _) -> do
  result <- foldM (go ptr) 0 [0, 2 .. span - 2]
  extra <-
    if even span
      then pure 0
      else
        peekByteOff ptr (span - 1) <&> \(w8 :: Word8) ->
          fromIntegral w8 `unsafeShiftL` ((span - 1) * 8)
  pure $ result + extra
  where
    go :: Ptr CChar -> Integer -> Int -> IO Integer
    go ptr acc ix = do
      peeked :: Word16 <- peekByteOff ptr ix
      pure $
        if peeked == 0
          then acc
          else acc + fromIntegral peeked `unsafeShiftL` (ix * 16)

mkSmallBE :: Int -> Int -> ByteString -> Integer
mkSmallBE msIndex span bs = unsafeDupablePerformIO . useAsCStringLen bs $ \(ptr, len) -> do
  result <- foldM (go ptr len) 0 [length bs - 2, length bs - 4 .. msIndex]
  extra <-
    if even span
      then pure 0
      else
        peekByteOff ptr msIndex <&> \(w8 :: Word8) ->
          fromIntegral w8 `unsafeShiftL` ((span - 1) * 8)
  pure $ result + extra
  where
    go :: Ptr CChar -> Int -> Integer -> Int -> IO Integer
    go ptr len acc ix = do
      peekedBackwards :: Word16 <- peekByteOff ptr ix
      pure $
        if peekedBackwards == 0
          then acc
          else
            let peeked = byteSwap16 peeked
             in acc + fromIntegral peeked `unsafeShiftL` ((len - ix - 2) * 16)

mkBigLE :: Int -> ByteString -> Integer
mkBigLE span bs =
  let requiredWords = case span `quotRem` bytesPerWord of
        (q, 0) -> q
        (q, _) -> q + 1
   in unsafeDupablePerformIO . useAsCStringLen bs $ \(ptr, _) -> do
        integerRep@(MutableByteArray ir#) <- newByteArray (requiredWords * bytesPerWord)
        getSizeofMutableByteArray integerRep >>= \len -> fillByteArray integerRep 0 len 0
        copyBytes' ir# (castPtr ptr) span
        ByteArray frozen# <- unsafeFreezeByteArray integerRep
        pure $ IP frozen#

mkBigBE :: Int -> Int -> ByteString -> Integer
mkBigBE start span bs =
  let requiredWords = case span `quotRem` bytesPerWord of
        (q, 0) -> q
        (q, _) -> q + 1
   in unsafeDupablePerformIO . useAsCStringLen bs $ \(ptr, len) -> do
        integerRep@(MutableByteArray ir#) <- newByteArray (requiredWords * bytesPerWord)
        getSizeofMutableByteArray integerRep >>= \len' -> do fillByteArray integerRep 0 len' 0
        copyBytesReverse' ir# (plusPtr ptr start) span len
        ByteArray frozen# <- unsafeFreezeByteArray integerRep
        pure $ IP frozen#
