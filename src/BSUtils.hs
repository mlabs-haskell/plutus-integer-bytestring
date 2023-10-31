{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module BSUtils (stripStartZeroes, stripEndZeroes) where

import Control.Applicative (pure)
import Data.Bool (otherwise)
import Data.ByteString
  ( ByteString,
    drop,
    empty,
    null,
    take,
    useAsCStringLen,
  )
import Data.Eq ((==))
import Data.Function (($))
import Foreign.C.Types (CSize (CSize), CUChar)
import Foreign.Ptr (Ptr, castPtr)
import GHC.Num ((+), (-))
import GHC.Real (fromIntegral)
import System.IO.Unsafe (unsafeDupablePerformIO)

-- | Removes any zero bytes at the start of the input, up to the first
-- (lowest-index) non-zero byte.
--
-- = Laws
--
-- 1. @'stripStartZeroes' 'empty'@ @=@ @'empty'@
-- 2. @'stripStartZeroes' ('Data.ByteString.cons' w8 bs)@ @=@ @if w8 '==' 0 then
--    'stripStartZeroes' bs else 'Data.ByteString.cons' w8 bs@
{-# INLINEABLE stripStartZeroes #-}
stripStartZeroes :: ByteString -> ByteString
stripStartZeroes bs
  | null bs = bs
  | otherwise = unsafeDupablePerformIO $ useAsCStringLen bs $ \(ptr, len) -> do
      let cucharPtr :: Ptr CUChar = castPtr ptr
      let csizeLen :: CSize = fromIntegral len
      let firstNonZeroIndex = cFindFirstNonZero cucharPtr csizeLen
      pure $
        if firstNonZeroIndex == csizeLen
          then empty
          else drop (fromIntegral $ firstNonZeroIndex - 1) bs

-- | Removes any zero bytes at the end of the input, up to the last
-- (highest-index) non-zero byte.
--
-- = Laws
--
-- 1. @'stripEndZeroes' 'Data.ByteString.empty'@ @=@ @'Data.ByteString.empty'@
-- 2. @'stripEndZeroes'
stripEndZeroes :: ByteString -> ByteString
stripEndZeroes bs
  | null bs = bs
  | otherwise = unsafeDupablePerformIO $ useAsCStringLen bs $ \(ptr, len) -> do
      let cucharPtr :: Ptr CUChar = castPtr ptr
      let csizeLen :: CSize = fromIntegral len
      let lastNonZeroIndex = cFindLastNonZero cucharPtr csizeLen
      pure $
        if lastNonZeroIndex == csizeLen
          then empty
          else take (fromIntegral $ lastNonZeroIndex + 1) bs

-- Helpers

foreign import capi "bsutils.h find_first_nonzero"
  cFindFirstNonZero ::
    Ptr CUChar ->
    CSize ->
    CSize

foreign import capi "bsutils.h find_last_nonzero"
  cFindLastNonZero ::
    Ptr CUChar ->
    CSize ->
    CSize
