{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IndexedByteString
  ( IndexedByteString,
    toTestData,
    IndexedByteStringMany,
    toTestDataMany,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Kind (Type)
import HexByteString (HexByteString (HexByteString))
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink),
    Arbitrary1 (liftArbitrary, liftShrink),
    CoArbitrary (coarbitrary),
    Function (function),
    NonEmptyList (NonEmpty),
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
    i <- chooseInt (0, (len * 8) - 1)
    pure . IndexedByteString (HexByteString consed) . fromIntegral $ i
  shrink (IndexedByteString bs i) = do
    HexByteString bs' <- shrink bs
    i' <- shrink i
    guard (not . BS.null $ bs')
    guard (i >= 0)
    let bitLen = (fromIntegral . BS.length $ bs') * 8
    guard (i < bitLen)
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

data IndexedByteStringMany = IndexedByteStringMany HexByteString [(Integer, Bool)]
  deriving stock (Eq, Ord, Show)

-- For some ungodly reason, this instance is missing
instance Arbitrary1 NonEmptyList where
  liftArbitrary gen = do
    x <- gen
    xs <- liftArbitrary gen
    pure . NonEmpty $ x : xs
  liftShrink ::
    forall (a :: Type).
    (a -> [a]) ->
    NonEmptyList a ->
    [NonEmptyList a]
  liftShrink shr (NonEmpty xs) =
    NonEmpty <$> case xs of
      [] -> [] -- technically impossible
      (y : ys) -> do
        withX y ys <|> withoutX ys
    where
      withX :: a -> [a] -> [[a]]
      withX y ys = do
        y' <- shr y
        ys' <- liftShrink shr ys
        pure (y' : ys')
      withoutX :: [a] -> [[a]]
      withoutX = \case
        [] -> []
        (y : ys) -> do
          y' <- shr y
          ys' <- liftShrink shr ys
          pure (y' : ys')

instance Arbitrary IndexedByteStringMany where
  arbitrary = do
    w8 <- arbitrary
    HexByteString bs <- arbitrary
    let consed = BS.cons w8 bs
    let bitLen = BS.length consed * 8
    NonEmpty ixes <- liftArbitrary ((,) <$> (fromIntegral <$> chooseInt (0, bitLen - 1)) <*> arbitrary)
    pure $ IndexedByteStringMany (HexByteString consed) ixes
  shrink (IndexedByteStringMany bs ixes) = do
    hbs@(HexByteString bs') <- shrink bs
    guard (not . BS.null $ bs')
    let bitLen = fromIntegral (BS.length bs') * 8
    ixes' <- liftShrink (go bitLen) ixes
    pure . IndexedByteStringMany hbs $ ixes'
    where
      go :: Integer -> (Integer, Bool) -> [(Integer, Bool)]
      go bitLen (i, b) = do
        i' <- shrink i
        b' <- shrink b
        guard (i' >= 0)
        guard (i' < bitLen)
        pure (i', b')

instance CoArbitrary IndexedByteStringMany where
  coarbitrary (IndexedByteStringMany bs ixes) =
    coarbitrary bs . coarbitrary ixes

instance Function IndexedByteStringMany where
  function =
    functionMap
      (\(IndexedByteStringMany bs ixes) -> (bs, ixes))
      (uncurry IndexedByteStringMany)

toTestDataMany :: IndexedByteStringMany -> (ByteString, [(Integer, Bool)])
toTestDataMany (IndexedByteStringMany (HexByteString bs) ixes) = (bs, ixes)
