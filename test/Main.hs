{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import CTConstants (bytesPerWord)
import Control.Category ((.))
import Data.ByteString (length, uncons, unsnoc)
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Maybe (Maybe (Just))
import Data.Ord (max, (<), (>))
import Data.Semigroup ((<>))
import Data.Tuple (fst, snd)
import GHC.ByteOrder (ByteOrder (BigEndian, LittleEndian))
import GHC.Real (fromIntegral, rem)
import Helpers (hexByteString)
import Naive (toByteString)
import SuitableInteger (countBytes, toInteger)
import System.IO (IO)
import Test.QuickCheck
  ( Property,
    classify,
    counterexample,
    property,
    (.&.),
    (===),
  )
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
  defaultMain . adjustOption moreTests . testGroup "Properties" $
    [ testGroup
        "toByteString"
        [ toByteStringProp1,
          toByteStringProp2,
          minimalByteCountCorrect
        ]
    ]
  where
    -- By default, QuickCheck (and hence, tasty-quickcheck) runs only 100 tests.
    -- As this is far too few to draw any useful conclusions, or avoid false
    -- negatives (no errors found, but problems remain), we raise this number to
    -- at least 10,000, while allowing more to be set via CLI if required.
    moreTests :: QuickCheckTests -> QuickCheckTests
    moreTests = max 10_000

-- Properties

toByteStringProp1 :: TestTree
toByteStringProp1 = testProperty propName . property $ \d i ->
  let i' = toInteger i
      expected = Just (fromIntegral $ i' `rem` 256)
      converted = toByteString d LittleEndian i'
      actual = fst <$> uncons converted
   in counterexample ("Converted ByteString: " <> hexByteString converted)
        . classifyTBSCodePath (countBytes i')
        $ expected === actual
  where
    propName :: TestName
    propName =
      "fst <$> uncons (toByteString d LittleEndian i) "
        <> "="
        <> " Just (fromIntegral $ i `rem` 256)"

toByteStringProp2 :: TestTree
toByteStringProp2 = testProperty propName . property $ \i d ->
  let i' = toInteger i
      expected = Just (fromIntegral $ i' `rem` 256)
      converted = toByteString d BigEndian i'
      convertedLE = toByteString d LittleEndian i'
      actual = snd <$> unsnoc converted
   in counterexample ("ConvertedByteString: " <> hexByteString converted)
        . counterexample ("Equivalent LE: " <> hexByteString convertedLE)
        . classifyTBSCodePath (countBytes i')
        $ expected === actual
  where
    propName :: TestName
    propName =
      "snd <$> unsnoc (toByteString d BigEndian i) "
        <> "="
        <> " Just (fromIntegral $ i `rem` 256)"

minimalByteCountCorrect :: TestTree
minimalByteCountCorrect = testProperty propName . property $ \i ->
  let i' = toInteger i
      expected = countBytes i'
      actualLE = length (toByteString 0 LittleEndian i')
      actualBE = length (toByteString 0 BigEndian i')
   in classifyTBSCodePath expected $
        expected === actualLE .&. expected === actualBE
  where
    propName :: TestName
    propName =
      "countBytes i"
        <> " = "
        <> "length (toByteString 0 LittleEndian i)"
        <> " = "
        <> "length (toByteString 0 BigEndian i)"

-- Helpers

-- To ensure that all possible paths for toByteString are being exercised
-- (namely, all three cases we currently split the implementation into), we
-- provide a percentage breakdown of how many generated cases fall into which
-- code path.
classifyTBSCodePath :: Int -> Property -> Property
classifyTBSCodePath counted =
  classify (counted < bytesPerWord) "need less than one word's worth of bytes"
    . classify (counted == bytesPerWord) "need exactly one word's worth of bytes"
    . classify (counted > bytesPerWord) "need more than one word's worth of bytes"
