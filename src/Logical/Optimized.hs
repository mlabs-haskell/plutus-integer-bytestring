{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Logical.Optimized
  ( complement,
    and,
    or,
    xor,
    setBit,
    setBits,
    popcount,
    findFirstSet,
    findFirstSetSkip,
  )
where

import Control.Category ((.))
import Data.Bits ((.&.), (.|.))
import Data.Bits qualified as Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.Foldable (for_, traverse_)
import Data.Word (Word64, Word8, byteSwap64)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable
  ( peekByteOff,
    peekElemOff,
    pokeByteOff,
    pokeElemOff,
  )
import System.IO.Unsafe (unsafeDupablePerformIO)
import Prelude
  ( Bool,
    IO,
    Int,
    Integer,
    error,
    fromInteger,
    fromIntegral,
    otherwise,
    pure,
    quot,
    quotRem,
    rem,
    ($),
    (*),
    (+),
    (-),
    (<),
    (<=),
    (==),
    (>=),
  )

{-# INLINEABLE complement #-}
complement :: ByteString -> ByteString
complement bs = unsafeDupablePerformIO . BS.useAsCStringLen bs $ \(srcPtr, len) -> do
  let (bigStrides, littleStrides) = len `quotRem` 8
  let offset = bigStrides * 8
  BSI.create len $ \dstPtr -> do
    let bigSrcPtr :: Ptr Word64 = castPtr srcPtr
    let bigDstPtr :: Ptr Word64 = castPtr dstPtr
    for_ [0 .. bigStrides - 1] $ \i -> do
      w64 <- peekElemOff bigSrcPtr i
      pokeElemOff bigDstPtr i . Bits.complement $ w64
    let smallSrcPtr :: Ptr Word8 = plusPtr srcPtr offset
    let smallDstPtr :: Ptr Word8 = plusPtr dstPtr offset
    for_ [0 .. littleStrides - 1] $ \i -> do
      w8 <- peekElemOff smallSrcPtr i
      pokeElemOff smallDstPtr i . Bits.complement $ w8

{-# INLINEABLE and #-}
and :: Bool -> ByteString -> ByteString -> ByteString
and shouldPad bs1 bs2 =
  let (shorter, longer) = if BS.length bs1 < BS.length bs2 then (bs1, bs2) else (bs2, bs1)
      (toCopy, toTraverse) = if shouldPad then (longer, shorter) else (shorter, longer)
   in go toCopy toTraverse (BS.length shorter)
  where
    go :: ByteString -> ByteString -> Int -> ByteString
    go toCopy toTraverse traverseLen =
      unsafeDupablePerformIO . BS.useAsCStringLen toCopy $ \(copyPtr, copyLen) ->
        BS.useAsCString toTraverse $ \traversePtr -> do
          BSI.create copyLen $ \dstPtr -> do
            copyBytes dstPtr (castPtr copyPtr) copyLen
            let (bigStrides, littleStrides) = traverseLen `quotRem` 8
            let offset = bigStrides * 8
            let bigDstPtr :: Ptr Word64 = castPtr dstPtr
            let bigTraversePtr :: Ptr Word64 = castPtr traversePtr
            for_ [0 .. bigStrides - 1] $ \i -> do
              w64_1 <- peekElemOff bigDstPtr i
              w64_2 <- peekElemOff bigTraversePtr i
              pokeElemOff bigDstPtr i $ w64_1 .&. w64_2
            let smallDstPtr :: Ptr Word8 = plusPtr dstPtr offset
            let smallTraversePtr :: Ptr Word8 = plusPtr traversePtr offset
            for_ [0 .. littleStrides - 1] $ \i -> do
              w8_1 <- peekElemOff smallDstPtr i
              w8_2 <- peekElemOff smallTraversePtr i
              pokeElemOff smallDstPtr i $ w8_1 .&. w8_2

{-# INLINEABLE or #-}
or :: Bool -> ByteString -> ByteString -> ByteString
or shouldPad bs1 bs2 =
  let (shorter, longer) = if BS.length bs1 < BS.length bs2 then (bs1, bs2) else (bs2, bs1)
      (toCopy, toTraverse) = if shouldPad then (longer, shorter) else (shorter, longer)
   in go toCopy toTraverse (BS.length shorter)
  where
    go :: ByteString -> ByteString -> Int -> ByteString
    go toCopy toTraverse traverseLen =
      unsafeDupablePerformIO . BS.useAsCStringLen toCopy $ \(copyPtr, copyLen) ->
        BS.useAsCString toTraverse $ \traversePtr -> do
          BSI.create copyLen $ \dstPtr -> do
            copyBytes dstPtr (castPtr copyPtr) copyLen
            let (bigStrides, littleStrides) = traverseLen `quotRem` 8
            let offset = bigStrides * 8
            let bigDstPtr :: Ptr Word64 = castPtr dstPtr
            let bigTraversePtr :: Ptr Word64 = castPtr traversePtr
            for_ [0 .. bigStrides - 1] $ \i -> do
              w64_1 <- peekElemOff bigDstPtr i
              w64_2 <- peekElemOff bigTraversePtr i
              pokeElemOff bigDstPtr i $ w64_1 .|. w64_2
            let smallDstPtr :: Ptr Word8 = plusPtr dstPtr offset
            let smallTraversePtr :: Ptr Word8 = plusPtr traversePtr offset
            for_ [0 .. littleStrides - 1] $ \i -> do
              w8_1 <- peekElemOff smallDstPtr i
              w8_2 <- peekElemOff smallTraversePtr i
              pokeElemOff smallDstPtr i $ w8_1 .|. w8_2

{-# INLINEABLE xor #-}
xor :: Bool -> ByteString -> ByteString -> ByteString
xor shouldPad bs1 bs2 =
  let (shorter, longer) = if BS.length bs1 < BS.length bs2 then (bs1, bs2) else (bs2, bs1)
      (toCopy, toTraverse) = if shouldPad then (longer, shorter) else (shorter, longer)
   in go toCopy toTraverse (BS.length shorter)
  where
    go :: ByteString -> ByteString -> Int -> ByteString
    go toCopy toTraverse traverseLen =
      unsafeDupablePerformIO . BS.useAsCStringLen toCopy $ \(copyPtr, copyLen) ->
        BS.useAsCString toTraverse $ \traversePtr -> do
          BSI.create copyLen $ \dstPtr -> do
            copyBytes dstPtr (castPtr copyPtr) copyLen
            let (bigStrides, littleStrides) = traverseLen `quotRem` 8
            let offset = bigStrides * 8
            let bigDstPtr :: Ptr Word64 = castPtr dstPtr
            let bigTraversePtr :: Ptr Word64 = castPtr traversePtr
            for_ [0 .. bigStrides - 1] $ \i -> do
              w64_1 <- peekElemOff bigDstPtr i
              w64_2 <- peekElemOff bigTraversePtr i
              pokeElemOff bigDstPtr i $ Bits.xor w64_1 w64_2
            let smallDstPtr :: Ptr Word8 = plusPtr dstPtr offset
            let smallTraversePtr :: Ptr Word8 = plusPtr traversePtr offset
            for_ [0 .. littleStrides - 1] $ \i -> do
              w8_1 <- peekElemOff smallDstPtr i
              w8_2 <- peekElemOff smallTraversePtr i
              pokeElemOff smallDstPtr i $ Bits.xor w8_1 w8_2

{-# INLINEABLE setBit #-}
setBit :: ByteString -> Integer -> Bool -> ByteString
setBit bs ix b =
  let ixInt = fromIntegral ix
      len = BS.length bs
      bitLen = len * 8
   in if
          | ixInt < 0 -> error "setBit: negative index"
          | ixInt >= bitLen -> error "setBit: index out of bounds"
          | otherwise -> unsafeDupablePerformIO . BS.useAsCString bs $ \srcPtr -> do
              BSI.create len $ \dstPtr -> do
                copyBytes dstPtr (castPtr srcPtr) len
                let (bigIx, littleIx) = ixInt `quotRem` 8
                let flipIx = len - bigIx - 1
                w8 :: Word8 <- peekByteOff srcPtr flipIx
                if b
                  then pokeByteOff dstPtr flipIx . Bits.setBit w8 $ littleIx
                  else pokeByteOff dstPtr flipIx . Bits.clearBit w8 $ littleIx

{-# INLINEABLE setBits #-}
setBits :: ByteString -> [(Integer, Bool)] -> ByteString
setBits bs changelist = unsafeDupablePerformIO . BS.useAsCString bs $ \srcPtr ->
  BSI.create len $ \dstPtr -> do
    copyBytes dstPtr (castPtr srcPtr) len
    traverse_ (go dstPtr) changelist
  where
    go :: Ptr Word8 -> (Integer, Bool) -> IO ()
    go ptr (i, b) =
      let intI = fromInteger i
       in if
              | intI < 0 -> error "setBits: negative index"
              | intI >= bitLen -> error "setBits: index out of bounds"
              | otherwise -> do
                  let (bigIx, littleIx) = intI `quotRem` 8
                  let flipIx = len - bigIx - 1
                  w8 :: Word8 <- peekByteOff ptr flipIx
                  pokeByteOff ptr flipIx $
                    if b
                      then Bits.setBit w8 littleIx
                      else Bits.clearBit w8 littleIx
    len :: Int
    len = BS.length bs
    bitLen :: Int
    bitLen = len * 8

{-# INLINEABLE popcount #-}
popcount :: ByteString -> Int
popcount bs = unsafeDupablePerformIO . BS.useAsCString bs $ \srcPtr -> do
  let bigSrcPtr :: Ptr Word64 = castPtr srcPtr
  let smallSrcPtr :: Ptr Word8 = plusPtr srcPtr offset
  goBig bigSrcPtr smallSrcPtr 0 0
  where
    len :: Int
    !len = BS.length bs
    bigStrides :: Int
    !bigStrides = len `quot` 8
    smallStrides :: Int
    !smallStrides = len `rem` 8
    offset :: Int
    !offset = bigStrides * 8
    goBig :: Ptr Word64 -> Ptr Word8 -> Int -> Int -> IO Int
    goBig !bigSrcPtr !smallSrcPtr !acc !bigIx
      | bigIx == bigStrides = goSmall smallSrcPtr acc 0
      | otherwise = do
          !w64 <- peekElemOff bigSrcPtr bigIx
          goBig bigSrcPtr smallSrcPtr (acc + Bits.popCount w64) (bigIx + 1)
    goSmall :: Ptr Word8 -> Int -> Int -> IO Int
    goSmall !smallSrcPtr !acc !smallIx
      | smallIx == smallStrides = pure acc
      | otherwise = do
          !w8 <- peekElemOff smallSrcPtr smallIx
          goSmall smallSrcPtr (acc + Bits.popCount w8) (smallIx + 1)

{-# INLINEABLE findFirstSet #-}
findFirstSet :: ByteString -> Int
findFirstSet bs = unsafeDupablePerformIO . BS.useAsCString bs $ \srcPtr -> do
  let bigSrcPtr :: Ptr Word64 = castPtr srcPtr
  goBig bigSrcPtr 0 (len - 8)
  where
    goBig :: Ptr Word64 -> Int -> Int -> IO Int
    goBig !bigSrcPtr !acc !byteIx
      | byteIx >= 0 = do
          !(w64 :: Word64) <- peekByteOff bigSrcPtr byteIx
          if w64 == 0x0
            then goBig bigSrcPtr (acc + 64) (byteIx - 8)
            else goSmall (castPtr bigSrcPtr) acc (byteIx + 7)
      | byteIx <= (-8) = pure (-1)
      | otherwise = goSmall (castPtr bigSrcPtr) 0 (8 + byteIx - 1)
    goSmall :: Ptr Word8 -> Int -> Int -> IO Int
    goSmall !smallSrcPtr !acc !byteIx
      | byteIx < 0 = pure (-1)
      | otherwise = do
          !(w8 :: Word8) <- peekByteOff smallSrcPtr byteIx
          let !counted = Bits.countTrailingZeros w8
          let !newAcc = acc + counted
          if counted == 8
            then goSmall smallSrcPtr newAcc (byteIx - 1)
            else pure newAcc
    len :: Int
    !len = BS.length bs

{-# INLINEABLE findFirstSetSkip #-}
findFirstSetSkip :: ByteString -> Int
findFirstSetSkip bs = unsafeDupablePerformIO . BS.useAsCString bs $ \srcPtr -> do
  let bigSrcPtr :: Ptr Word64 = castPtr srcPtr
  goBig bigSrcPtr 0 (len - 8)
  where
    goBig :: Ptr Word64 -> Int -> Int -> IO Int
    goBig !bigSrcPtr !acc !byteIx
      | byteIx >= 0 = do
          !(w64 :: Word64) <- peekByteOff bigSrcPtr byteIx
          if w64 == 0x0
            then goBig bigSrcPtr (acc + 64) (byteIx - 8)
            else do
              let !counted = Bits.countTrailingZeros . byteSwap64 $ w64
              pure $ acc + counted
      | byteIx <= (-8) = pure (-1)
      | otherwise = goSmall (castPtr bigSrcPtr) 0 (8 + byteIx - 1)
    goSmall :: Ptr Word8 -> Int -> Int -> IO Int
    goSmall !smallSrcPtr !acc !byteIx
      | byteIx < 0 = pure (-1)
      | otherwise = do
          !(w8 :: Word8) <- peekByteOff smallSrcPtr byteIx
          let !counted = Bits.countTrailingZeros w8
          let !newAcc = acc + counted
          if counted == 8
            then goSmall smallSrcPtr newAcc (byteIx - 1)
            else pure newAcc
    len :: Int
    !len = BS.length bs
