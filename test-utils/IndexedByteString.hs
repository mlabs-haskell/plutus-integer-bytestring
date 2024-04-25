{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}

module IndexedByteString (IndexedByteString, toTestData) where

import Control.Monad (guard)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import HexByteString (HexByteString (HexByteString))
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink),
    CoArbitrary (coarbitrary),
    Function (function),
    chooseInt,
    functionMap,
  )

data IndexedByteString = IndexedByteString HexByteString Integer
  deriving stock (Eq, Ord, Show)

instance Arbitrary IndexedByteString where
  arbitrary = do
    w8 <- arbitrary
    HexByteString bs <- arbitrary
    let consed = BS.cons w8 bs
    let len = BS.length consed
    i <- chooseInt (0, len - 1)
    pure . IndexedByteString (HexByteString consed) . fromIntegral $ i
  shrink (IndexedByteString bs i) = do
    HexByteString bs' <- shrink bs
    i' <- shrink i
    guard (not . BS.null $ bs')
    guard (i >= 0)
    guard (i < (fromIntegral . BS.length $ bs'))
    pure . IndexedByteString (HexByteString bs') $ i'

instance CoArbitrary IndexedByteString where
  coarbitrary (IndexedByteString bs i) =
    coarbitrary bs . coarbitrary i

instance Function IndexedByteString where
  function =
    functionMap
      (\(IndexedByteString bs i) -> (bs, i))
      (uncurry IndexedByteString)

toTestData :: IndexedByteString -> (ByteString, Integer)
toTestData (IndexedByteString (HexByteString bs) i) = (bs, i)
