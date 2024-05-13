{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Control.Category ((.))
import Data.Function (($))
import Data.Ord (max)
import HexByteString (HexByteString (HexByteString))
import IndexedByteString (toTestData, toTestDataMany)
import Logical.Naive qualified as Naive
import Logical.Optimized qualified as Optimized
import System.IO (IO)
import Test.QuickCheck
  ( Property,
    property,
    (===),
  )
import Test.Tasty
  ( adjustOption,
    defaultMain,
    testGroup,
  )
import Test.Tasty.QuickCheck (QuickCheckTests, testProperty)

main :: IO ()
main =
  defaultMain . adjustOption moreTests . testGroup "Consistency properties" $
    [ testProperty "complement" complementIsConsistent,
      testProperty "and" andIsConsistent,
      testProperty "or" orIsConsistent,
      testProperty "xor" xorIsConsistent,
      testProperty "setBit" setBitIsConsistent,
      testProperty "setBits" setBitsIsConsistent,
      testProperty "popcount" popcountIsConsistent,
      testProperty "findFirstSet" findFirstSetIsConsistent,
      testProperty "findFirstTestSkip" findFirstSetSkipIsConsistent
    ]
  where
    -- By default, QuickCheck (and hence, tasty-quickcheck) runs only 100 tests.
    -- As this is far too few to draw any useful conclusions, or avoid false
    -- negatives (no errors found, but problems remain), we raise this number to
    -- at least 100,000, while allowing more to be set via CLI if required.
    moreTests :: QuickCheckTests -> QuickCheckTests
    moreTests = max 100_000

-- Properties

findFirstSetSkipIsConsistent :: Property
findFirstSetSkipIsConsistent = property $ \(HexByteString bs) ->
  Naive.findFirstSet bs === Optimized.findFirstSetSkip bs

findFirstSetIsConsistent :: Property
findFirstSetIsConsistent = property $ \(HexByteString bs) ->
  Naive.findFirstSet bs === Optimized.findFirstSet bs

popcountIsConsistent :: Property
popcountIsConsistent = property $ \(HexByteString bs) ->
  Naive.popcount bs === Optimized.popcount bs

complementIsConsistent :: Property
complementIsConsistent = property $ \(HexByteString bs) ->
  Naive.complement bs === Optimized.complement bs

andIsConsistent :: Property
andIsConsistent = property $ \(shouldPad, HexByteString bs1, HexByteString bs2) ->
  HexByteString (Naive.and shouldPad bs1 bs2)
    === HexByteString (Optimized.and shouldPad bs1 bs2)

orIsConsistent :: Property
orIsConsistent = property $ \(shouldPad, HexByteString bs1, HexByteString bs2) ->
  HexByteString (Naive.or shouldPad bs1 bs2)
    === HexByteString (Optimized.or shouldPad bs1 bs2)

xorIsConsistent :: Property
xorIsConsistent = property $ \(shouldPad, HexByteString bs1, HexByteString bs2) ->
  HexByteString (Naive.xor shouldPad bs1 bs2)
    === HexByteString (Optimized.xor shouldPad bs1 bs2)

setBitIsConsistent :: Property
setBitIsConsistent = property $ \(ibs, b) ->
  let (bs, i) = toTestData ibs
   in HexByteString (Naive.setBit bs i b)
        === HexByteString (Optimized.setBit bs i b)

setBitsIsConsistent :: Property
setBitsIsConsistent = property $ \x ->
  let (bs, ixes) = toTestDataMany x
   in HexByteString (Naive.setBits bs ixes)
        === HexByteString (Optimized.setBits bs ixes)
