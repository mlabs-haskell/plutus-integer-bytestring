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
    shift,
    rotate,
  )
where

import Control.Category ((.))
import Control.Monad (unless, when)
import Data.Bits ((.&.), (.|.))
import Data.Bits qualified as Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.Foldable (for_, traverse_)
import Data.Word (Word64, Word8, byteSwap64)
import Foreign.Marshal.Utils (copyBytes, fillBytes)
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
    abs,
    error,
    fromInteger,
    fromIntegral,
    otherwise,
    pure,
    quot,
    quotRem,
    rem,
    signum,
    ($),
    (*),
    (+),
    (-),
    (<),
    (<=),
    (==),
    (>),
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

{-# INLINEABLE shift #-}
shift :: ByteString -> Int -> ByteString
shift bs bitMove
  | BS.null bs = bs
  | bitMove == 0 = bs
  | otherwise = unsafeDupablePerformIO . BS.useAsCString bs $ \srcPtr ->
      BSI.create len $ \dstPtr -> do
        let magnitude = abs bitMove
        fillBytes dstPtr 0x00 len
        unless (magnitude >= bitLen) $ do
          let (bigShift, smallShift) = magnitude `quotRem` 8
          case signum bitMove of
            (-1) -> negativeShift (castPtr srcPtr) dstPtr bigShift smallShift
            _ -> positiveShift (castPtr srcPtr) dstPtr bigShift smallShift
  where
    len :: Int
    !len = BS.length bs
    bitLen :: Int
    !bitLen = len * 8
    negativeShift :: Ptr Word8 -> Ptr Word8 -> Int -> Int -> IO ()
    negativeShift srcPtr dstPtr bigShift smallShift = do
      let copyDstPtr = plusPtr dstPtr bigShift
      let copyLen = len - bigShift
      copyBytes copyDstPtr srcPtr copyLen
      when (smallShift > 0) $ do
        let !invSmallShift = 8 - smallShift
        let !mask = 0xFF `Bits.unsafeShiftR` invSmallShift
        for_ [len - 1, len - 2 .. len - copyLen] $ \byteIx -> do
          !(currentByte :: Word8) <- peekByteOff dstPtr byteIx
          !(prevByte :: Word8) <- peekByteOff dstPtr (byteIx - 1)
          let !prevOverflowBits = prevByte .&. mask
          let !newCurrentByte =
                (currentByte `Bits.unsafeShiftR` smallShift)
                  .|. (prevOverflowBits `Bits.unsafeShiftL` invSmallShift)
          pokeByteOff dstPtr byteIx newCurrentByte
        !(firstByte :: Word8) <- peekByteOff dstPtr (len - copyLen - 1)
        pokeByteOff dstPtr (len - copyLen - 1) (firstByte `Bits.unsafeShiftR` smallShift)
    positiveShift :: Ptr Word8 -> Ptr Word8 -> Int -> Int -> IO ()
    positiveShift srcPtr dstPtr bigShift smallShift = do
      let copySrcPtr = plusPtr srcPtr bigShift
      let copyLen = len - bigShift
      copyBytes dstPtr copySrcPtr copyLen
      when (smallShift > 0) $ do
        let !invSmallShift = 8 - smallShift
        let !mask = 0xFF `Bits.unsafeShiftL` invSmallShift
        for_ [0, 1 .. copyLen - 2] $ \byteIx -> do
          !(currentByte :: Word8) <- peekByteOff dstPtr byteIx
          !(nextByte :: Word8) <- peekByteOff dstPtr (byteIx + 1)
          let !nextOverflowBits = nextByte .&. mask
          let !newCurrentByte =
                (currentByte `Bits.unsafeShiftL` smallShift)
                  .|. (nextOverflowBits `Bits.unsafeShiftR` invSmallShift)
          pokeByteOff dstPtr byteIx newCurrentByte
        !(lastByte :: Word8) <- peekByteOff dstPtr (copyLen - 1)
        pokeByteOff dstPtr (copyLen - 1) (lastByte `Bits.unsafeShiftL` smallShift)

{-# INLINEABLE rotate #-}
rotate :: ByteString -> Int -> ByteString
rotate bs bitMove
  | BS.null bs = bs
  | otherwise =
      let !magnitude = abs bitMove
          !reducedMagnitude = magnitude `rem` bitLen
       in if reducedMagnitude == 0
            then bs
            else unsafeDupablePerformIO . BS.useAsCString bs $ \srcPtr ->
              BSI.create len $ \dstPtr -> do
                let (bigRotation, smallRotation) = reducedMagnitude `quotRem` 8
                case signum bitMove of
                  (-1) -> negativeRotate (castPtr srcPtr) dstPtr bigRotation smallRotation
                  _ -> positiveRotate (castPtr srcPtr) dstPtr bigRotation smallRotation
  where
    len :: Int
    !len = BS.length bs
    bitLen :: Int
    !bitLen = len * 8
    negativeRotate :: Ptr Word8 -> Ptr Word8 -> Int -> Int -> IO ()
    negativeRotate srcPtr dstPtr bigRotate smallRotate = do
      let copyStartDstPtr = plusPtr dstPtr bigRotate
      let copyStartLen = len - bigRotate
      copyBytes copyStartDstPtr srcPtr copyStartLen
      let copyEndSrcPtr = plusPtr srcPtr copyStartLen
      copyBytes dstPtr copyEndSrcPtr bigRotate
      when (smallRotate > 0) $ do
        let invSmallRotate = 8 - smallRotate
        let !mask = 0xFF `Bits.unsafeShiftR` invSmallRotate
        !(cloneLastByte :: Word8) <- peekByteOff dstPtr (len - 1)
        for_ [len - 1, len - 2 .. 1] $ \byteIx -> do
          !(currentByte :: Word8) <- peekByteOff dstPtr byteIx
          !(prevByte :: Word8) <- peekByteOff dstPtr (byteIx - 1)
          let !prevOverflowBits = prevByte .&. mask
          let !newCurrentByte =
                (currentByte `Bits.unsafeShiftR` smallRotate)
                  .|. (prevOverflowBits `Bits.unsafeShiftL` invSmallRotate)
          pokeByteOff dstPtr byteIx newCurrentByte
        !(firstByte :: Word8) <- peekByteOff dstPtr 0
        let !lastByteOverflow = cloneLastByte .&. mask
        let !newLastByte =
              (firstByte `Bits.unsafeShiftR` smallRotate)
                .|. (lastByteOverflow `Bits.unsafeShiftL` invSmallRotate)
        pokeByteOff dstPtr 0 newLastByte
    positiveRotate :: Ptr Word8 -> Ptr Word8 -> Int -> Int -> IO ()
    positiveRotate srcPtr dstPtr bigRotate smallRotate = do
      let copyStartSrcPtr = plusPtr srcPtr bigRotate
      let copyStartLen = len - bigRotate
      copyBytes dstPtr copyStartSrcPtr copyStartLen
      let copyEndDstPtr = plusPtr dstPtr copyStartLen
      copyBytes copyEndDstPtr srcPtr bigRotate
      when (smallRotate > 0) $ do
        let !invSmallRotate = 8 - smallRotate
        let !mask = 0xFF `Bits.unsafeShiftL` invSmallRotate
        !(cloneFirstByte :: Word8) <- peekByteOff dstPtr 0
        for_ [0, 1 .. len - 2] $ \byteIx -> do
          !(currentByte :: Word8) <- peekByteOff dstPtr byteIx
          !(nextByte :: Word8) <- peekByteOff dstPtr (byteIx + 1)
          let !nextOverflowBits = nextByte .&. mask
          let !newCurrentByte =
                (currentByte `Bits.unsafeShiftL` smallRotate)
                  .|. (nextOverflowBits `Bits.unsafeShiftR` invSmallRotate)
          pokeByteOff dstPtr byteIx newCurrentByte
        !(lastByte :: Word8) <- peekByteOff dstPtr (len - 1)
        let !firstOverflowBits = cloneFirstByte .&. mask
        let !newLastByte =
              (lastByte `Bits.unsafeShiftL` smallRotate)
                .|. (firstOverflowBits `Bits.unsafeShiftR` invSmallRotate)
        pokeByteOff dstPtr (len - 1) newLastByte
