{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import BSUtils (ptrReverse)
import BytesWithIndex (getBytes, getIndex)
import Control.Category ((.))
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (index, useAsCStringLen)
import Data.Eq ((==))
import Data.Function (($))
import Data.Ord (max)
import Data.Semigroup ((<>))
import Data.String (String)
import Data.Word (Word8)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (castPtr)
import Foreign.Storable (peekByteOff)
import GHC.Num ((-))
import Helpers (hexByte)
import System.IO (IO)
import Test.QuickCheck (property)
import Test.QuickCheck.Monadic (assertWith, monadicIO)
import Test.Tasty
  ( TestName,
    TestTree,
    adjustOption,
    defaultMain,
    testGroup,
  )
import Test.Tasty.QuickCheck (QuickCheckTests, testProperty)

main :: IO ()
main =
  defaultMain . adjustOption moreTests . testGroup "FFI utility properties" $
    [ swapIndexProperty
    ]
  where
    -- By default, QuickCheck (and hence, tasty-quickcheck) runs only 100 tests.
    -- As this is far too few to draw any useful conclusions, or avoid false
    -- negatives (no errors found, but problems remain), we raise this number to
    -- at least 10,000, while allowing more to be set via CLI if required.
    moreTests :: QuickCheckTests -> QuickCheckTests
    moreTests = max 10_000

-- Properties

swapIndexProperty :: TestTree
swapIndexProperty = testProperty propName . property $ \bi ->
  monadicIO $ do
    let bytes = getBytes bi
    let ix = getIndex bi
    let atOldPosition = index bytes ix
    atNewPosition :: Word8 <- liftIO . useAsCStringLen bytes $ \(srcPtr, len) ->
      -- Because ptrReverse works in-place, we make a temporary copy of bytes,
      -- reverse the copy, grab the byte at the corresponding index, then tear it
      -- down.
      allocaBytes len $ \dstPtr -> do
        copyBytes dstPtr srcPtr len
        ptrReverse (castPtr dstPtr) len
        let reversedIndex = len - 1 - ix
        peekByteOff dstPtr reversedIndex
    assertWith
      (atOldPosition == atNewPosition)
      (mismatchErrorMsg atOldPosition atNewPosition)
  where
    propName :: TestName
    propName = "Reversed byte blocks translate position i to position (len - i - 1)"
    mismatchErrorMsg :: Word8 -> Word8 -> String
    mismatchErrorMsg atOldPosition atNewPosition =
      "Bytes do not match.\nAt old position: "
        <> hexByte atOldPosition
        <> "\nAt new position: "
        <> hexByte atNewPosition
