{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Control.Category ((.))
import Data.Function (($))
import Data.Ord (max)
import HexByteString (HexByteString (HexByteString))
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
    [testProperty "complement" complementIsConsistent]
  where
    -- By default, QuickCheck (and hence, tasty-quickcheck) runs only 100 tests.
    -- As this is far too few to draw any useful conclusions, or avoid false
    -- negatives (no errors found, but problems remain), we raise this number to
    -- at least 10,000, while allowing more to be set via CLI if required.
    moreTests :: QuickCheckTests -> QuickCheckTests
    moreTests = max 10_000

-- Properties

complementIsConsistent :: Property
complementIsConsistent = property $ \(HexByteString bs) ->
  Naive.complement bs === Optimized.complement bs
