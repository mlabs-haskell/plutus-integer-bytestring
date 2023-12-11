{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Control.Category ((.))
import Data.ByteString
  ( cons,
    length,
    replicate,
    reverse,
    singleton,
    uncons,
  )
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Maybe (Maybe (Just))
import Data.Ord (max, (<), (>))
import Data.Semigroup ((<>))
import Data.Tuple (fst)
import Data.Word (Word)
import Foreign.Storable (sizeOf)
import GHC.ByteOrder (ByteOrder (BigEndian, LittleEndian))
import GHC.Num ((-))
import GHC.Real (fromIntegral, rem)
import Helpers (hexByteString)
import NEByteString qualified as NEBS
import SuitableInteger (countBytes, toInteger)
import System.IO (IO)
import Test.QuickCheck
  ( Positive (Positive),
    Property,
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
import WithBinary (fromByteString, toByteString)

main :: IO ()
main =
  defaultMain . adjustOption moreTests . testGroup "Naive implementation properties" $
    [ testGroup
        "toByteString"
        [ toByteStringProp1,
          toByteStringProp2,
          toByteStringProp3
        ],
      testGroup
        "fromByteString"
        [ fromByteStringProp1,
          fromByteStringProp2,
          fromByteStringProp3,
          fromByteStringProp4,
          fromByteStringProp5
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
toByteStringProp1 = testProperty propName . property $ \i ->
  let i' = toInteger i
      expected = Just (fromIntegral $ i' `rem` 256)
      converted = toByteString 0 LittleEndian i'
      actual = fst <$> uncons converted
   in counterexample ("Converted ByteString: " <> hexByteString converted)
        . classifyTBSCodePath (countBytes i')
        $ expected === actual
  where
    propName :: TestName
    propName =
      "fst <$> uncons (toByteString 0 LittleEndian i)"
        <> " = "
        <> "Just (fromIntegral $ i `rem` 256)"

toByteStringProp2 :: TestTree
toByteStringProp2 = testProperty propName . property $ \d w8 ->
  let i = fromIntegral w8
      expected = cons w8 (replicate (max 0 (d - 1)) 0)
      actual = toByteString d LittleEndian i
   in expected === actual
  where
    propName :: TestName
    propName =
      "toByteString d LittleEndian (fromIntegral w8)"
        <> " = "
        <> "cons w8 (replicate (max 0 (d - 1)) 0)"

toByteStringProp3 :: TestTree
toByteStringProp3 = testProperty propName . property $ \i ->
  let i' = toInteger i
      expected = reverse (toByteString 0 BigEndian i')
      actual = toByteString 0 LittleEndian i'
   in classifyTBSCodePath (countBytes i') $ expected === actual
  where
    propName :: TestName
    propName =
      "toByteString 0 LittleEndian i"
        <> " = "
        <> "reverse (toByteString 0 BigEndian i)"

fromByteStringProp1 :: TestTree
fromByteStringProp1 = testProperty propName . property $ \(Positive n) w8 ->
  let input = replicate n w8
      outcomeBE = fromByteString BigEndian input
      outcomeLE = fromByteString LittleEndian input
   in outcomeBE === outcomeLE
  where
    propName :: TestName
    propName =
      "fromByteString BigEndian (replicate n b)"
        <> " = "
        <> "fromByteString LittleEndian (replicate n b)"

fromByteStringProp2 :: TestTree
fromByteStringProp2 = testProperty propName . property $ \w8 ->
  let expected = fromIntegral w8
      input = singleton w8
      actualBE = fromByteString BigEndian input
      actualLE = fromByteString LittleEndian input
   in (expected === actualBE) .&. (expected === actualLE)
  where
    propName :: TestName
    propName =
      "fromByteString sbo (singleton w8)"
        <> " = "
        <> "fromIntegral w8"

fromByteStringProp3 :: TestTree
fromByteStringProp3 = testProperty propName . property $ \i ->
  let i' = toInteger i
      actualLE = fromByteString LittleEndian (toByteString 0 LittleEndian i')
      actualBE = fromByteString BigEndian (toByteString 0 BigEndian i')
   in classifyTBSCodePath (countBytes i') $ (i' === actualLE) .&. (i' === actualBE)
  where
    propName :: TestName
    propName =
      "fromByteString bo (toByteString 0 bo i)"
        <> " = "
        <> "i"

fromByteStringProp4 :: TestTree
fromByteStringProp4 = testProperty propName . property $ \neBS (Positive n) ->
  let bs = NEBS.toByteString neBS
      expected = fromByteString LittleEndian bs
      actual = fromByteString LittleEndian (bs <> replicate n 0)
   in expected === actual
  where
    propName :: TestName
    propName =
      "fromByteString LittleEndian (bs <> replicate n 0)"
        <> " = "
        <> "fromByteString LittleEndian bs"

fromByteStringProp5 :: TestTree
fromByteStringProp5 = testProperty propName . property $ \neBS ->
  let bs = NEBS.toByteString neBS
      actualLE = toByteString (length bs) LittleEndian (fromByteString LittleEndian bs)
      actualBE = toByteString (length bs) BigEndian (fromByteString BigEndian bs)
   in (bs === actualLE) .&. (bs == actualBE)
  where
    propName :: TestName
    propName = "toByteString (length bs) bo (fromByteString bo bs) = bs"

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

bytesPerWord :: Int
bytesPerWord = sizeOf (0x0 :: Word)
