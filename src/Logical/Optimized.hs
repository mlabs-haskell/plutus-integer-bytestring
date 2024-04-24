{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Logical.Optimized
  ( complement,
    and,
    or,
    xor,
  )
where

import Control.Category ((.))
import Data.Bits ((.&.), (.|.))
import Data.Bits qualified as Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.Foldable (for_)
import Data.Word (Word64, Word8)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (peekElemOff, pokeElemOff)
import System.IO.Unsafe (unsafeDupablePerformIO)
import Prelude
  ( Bool,
    Int,
    quotRem,
    ($),
    (*),
    (-),
    (<),
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