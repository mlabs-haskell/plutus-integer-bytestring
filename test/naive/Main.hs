{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Control.Category ((.))
import Control.Monad (guard)
import Data.ByteString
  ( ByteString,
    cons,
    length,
    replicate,
    reverse,
    singleton,
    uncons,
  )
import Data.ByteString qualified as BS
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Maybe (Maybe (Just))
import Data.Ord (max, (<), (>))
import Data.Semigroup ((<>))
import Data.Tuple (fst)
import Data.Word (Word, Word8)
import Foreign.Storable (sizeOf)
import GHC.ByteOrder (ByteOrder (BigEndian, LittleEndian))
import GHC.Num ((-))
import GHC.Real (fromIntegral, rem)
import Helpers (hexByteString)
import HexByteString (HexByteString (HexByteString))
import IndexedByteString (toTestData, toTestDataSplit)
import Laws
  ( abelianSemigroupLaws,
    absorbingLaws,
    idempotenceLaws,
    involuteLaws,
    monoidLaws,
  )
import Logical.Naive
  ( and,
    complement,
    getBit,
    or,
    setBits,
    xor,
  )
import Logical.Naive qualified as LNaive
import NEByteString qualified as NEBS
import Naive (fromByteString, toByteString)
import SuitableInteger (countBytes, toInteger)
import System.IO (IO)
import Test.QuickCheck
  ( Gen,
    NonNegative (NonNegative),
    Positive (Positive),
    Property,
    arbitrary,
    chooseInt,
    classify,
    counterexample,
    forAllShrink,
    property,
    shrink,
    (.&&.),
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
import Text.Show (show)
import Prelude
  ( Bool (False, True),
    Integer,
    pure,
    ($),
    (+),
    (==),
    (>=),
  )

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
        ],
      testGroup
        "replicate"
        [ testProperty "replicate 0 b = \"\"" . property $ \(w8 :: Word8) ->
            LNaive.replicate 0 (fromIntegral w8) === "",
          testProperty "replicate n b <> replicate m b = replicate (n + m) b" . property $
            \(NonNegative n, NonNegative m, w8 :: Word8) ->
              (LNaive.replicate n (fromIntegral w8) <> LNaive.replicate m (fromIntegral w8))
                === LNaive.replicate (n + m) (fromIntegral w8),
          testProperty "(replicate n b)[i] = b" . forAllShrink genReplicate shrinkReplicate $ \(w8, n, i) ->
            BS.index (LNaive.replicate n (fromIntegral w8)) i === w8
        ],
      testGroup
        "complement"
        [ complementProp,
          testProperty "De Morgan laws" deMorganLaws
        ],
      testGroup
        "and"
        [ testGroup "abelian semigroup (truncating)" . abelianSemigroupLaws $ and False,
          testGroup "idempotent semigroup (truncating)" . idempotenceLaws $ and False,
          testGroup "absorbing element (truncating)" . absorbingLaws (and False) $ "",
          testGroup "abelian semigroup (padding)" . abelianSemigroupLaws $ and True,
          testGroup "idempotent semigroup (padding)" . idempotenceLaws $ and True,
          testGroup "monoid (padding)" . monoidLaws (and True) $ "",
          testProperty "left distribution over OR (truncation)" . leftDist (and False) $ or False,
          testProperty "left distribution over XOR (truncation)" . leftDist (and False) $ xor False,
          testProperty "left distribution over itself (truncation)" . leftDist (and False) $ and False,
          testGroup "distribution over itself (padding)" . distributiveLaws $ and True
        ],
      testGroup
        "or"
        [ testGroup "abelian semigroup (truncating)" . abelianSemigroupLaws $ or False,
          testGroup "idempotent semigroup (truncating)" . idempotenceLaws $ or False,
          testGroup "absorbing element (truncating)" . absorbingLaws (or False) $ "",
          testGroup "abelian semigroup (padding)" . abelianSemigroupLaws $ or True,
          testGroup "idempotent semigroup (padding)" . idempotenceLaws $ or True,
          testGroup "monoid (padding)" . monoidLaws (or True) $ "",
          testProperty "left distribution over AND (truncation)" . leftDist (or False) $ and False,
          testProperty "left distribution over itself (truncation)" . leftDist (or False) $ or False,
          testGroup "distribution over itself (padding)" . distributiveLaws $ or True
        ],
      testGroup
        "xor"
        [ testGroup "abelian semigroup (truncating)" . abelianSemigroupLaws $ xor False,
          testGroup "absorbing element (truncating)" . absorbingLaws (xor False) $ "",
          testGroup "involute (truncating)" . involuteLaws $ xor False,
          testGroup "abelian semigroup (padding)" . abelianSemigroupLaws $ xor True,
          testGroup "monoid (padding)" . monoidLaws (xor True) $ "",
          testGroup "involute (padding)" . involuteLaws $ xor True
        ],
      testGroup
        "get-set"
        [ testProperty "Get-set" getSetLaw,
          testProperty "Set-get" setGetLaw,
          testProperty "Set-set" setSetLaw,
          testProperty "setBits bs [] = bs" setEmptyLaw,
          testProperty "setBits (setBits bs is) js = setBits bs (is <> js)" setConcatLaw
        ]
    ]
  where
    -- By default, QuickCheck (and hence, tasty-quickcheck) runs only 100 tests.
    -- As this is far too few to draw any useful conclusions, or avoid false
    -- negatives (no errors found, but problems remain), we raise this number to
    -- at least 10,000, while allowing more to be set via CLI if required.
    moreTests :: QuickCheckTests -> QuickCheckTests
    moreTests = max 10_000

-- Generators and shrinkers

genReplicate :: Gen (Word8, Integer, Int)
genReplicate = do
  w8 <- arbitrary
  Positive len <- arbitrary
  i <- chooseInt (0, fromIntegral len - 1)
  pure (w8, len, i)

shrinkReplicate :: (Word8, Integer, Int) -> [(Word8, Integer, Int)]
shrinkReplicate (w8, len, i) = do
  w8' <- shrink w8
  Positive len' <- shrink (Positive len)
  i' <- shrink i
  guard (i' >= 0)
  guard (i' < fromIntegral len')
  pure (w8', len', i')

-- Properties

leftDist ::
  (ByteString -> ByteString -> ByteString) ->
  (ByteString -> ByteString -> ByteString) ->
  Property
leftDist f g = property $ \(HexByteString x, HexByteString y, HexByteString z) ->
  HexByteString (f x (g y z)) === HexByteString (g (f x y) (f x z))

distributiveLaws ::
  (ByteString -> ByteString -> ByteString) ->
  [TestTree]
distributiveLaws f =
  [ testProperty "left" . leftDist f $ f,
    testProperty "right" . property $ \(HexByteString x, HexByteString y, HexByteString z) ->
      HexByteString (f (f x y) z) === HexByteString (f (f x z) (f y z))
  ]

deMorganLaws :: Property
deMorganLaws = property $ \(HexByteString bs1, HexByteString bs2, b) ->
  (complement (or b bs1 bs2) === and b (complement bs1) (complement bs2))
    .&&. (complement (and b bs1 bs2) === or b (complement bs1) (complement bs2))

setConcatLaw :: Property
setConcatLaw = property $ \sbs ->
  let (bs, is, js) = toTestDataSplit sbs
   in setBits (setBits bs is) js === setBits bs (is <> js)

setEmptyLaw :: Property
setEmptyLaw = property $ \(HexByteString bs) ->
  setBits bs [] === bs

getSetLaw :: Property
getSetLaw = property $ \ibs ->
  let (bs, i) = toTestData ibs
   in setBits bs [(i, getBit bs i)] === bs

setGetLaw :: Property
setGetLaw = property $ \(ibs, b) ->
  let (bs, i) = toTestData ibs
   in getBit (setBits bs [(i, b)]) i === b

setSetLaw :: Property
setSetLaw = property $ \(ibs, b1, b2) ->
  let (bs, i) = toTestData ibs
   in setBits bs [(i, b1), (i, b2)] === setBits bs [(i, b2)]

complementProp :: TestTree
complementProp = testProperty propName . property $ \(HexByteString bs) ->
  bs === (complement . complement $ bs)
  where
    propName :: TestName
    propName = "complement . complement = id"

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
toByteStringProp2 = testProperty propName . property $ \(NonNegative d) (Positive w8) ->
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
   in (expected === actualBE) .&&. (expected === actualLE)
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
   in classifyTBSCodePath (countBytes i') $ (i' === actualLE) .&&. (i' === actualBE)
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
      leNumber = fromByteString LittleEndian bs
      beNumber = fromByteString BigEndian bs
      actualLE = toByteString (length bs) LittleEndian leNumber
      actualBE = toByteString (length bs) BigEndian beNumber
   in counterexample ("BE number: " <> show beNumber)
        . counterexample ("LE number: " <> show leNumber)
        . counterexample ("Actual BE: " <> hexByteString actualBE)
        . counterexample ("Actual LE: " <> hexByteString actualLE)
        $ (bs === actualLE) .&&. (bs == actualBE)
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
