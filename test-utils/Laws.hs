{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Laws
  ( abelianSemigroupLaws,
    monoidLaws,
    idempotenceLaws,
    absorbingLaws,
    involuteLaws,
  )
where

import Data.Kind (Type)
import Test.QuickCheck
  ( Arbitrary,
    property,
    (.&&.),
    (===),
  )
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)

abelianSemigroupLaws ::
  forall (a :: Type).
  (Eq a, Arbitrary a, Show a) =>
  (a -> a -> a) ->
  [TestTree]
abelianSemigroupLaws f =
  [ testProperty "commutativity" . property $ \(x, y) ->
      f x y === f y x,
    testProperty "associativity" . property $ \(x, y, z) ->
      f x (f y z) === f (f x y) z
  ]

monoidLaws ::
  forall (a :: Type).
  (Eq a, Arbitrary a, Show a) =>
  (a -> a -> a) ->
  a ->
  [TestTree]
monoidLaws f identity =
  [ testProperty "identity" . property $ \x ->
      (f x identity === x)
        .&&. (x === f identity x)
  ]

idempotenceLaws ::
  forall (a :: Type).
  (Eq a, Arbitrary a, Show a) =>
  (a -> a -> a) ->
  [TestTree]
idempotenceLaws f =
  [ testProperty "idempotence" . property $ \x ->
      f x x === x
  ]

absorbingLaws ::
  forall (a :: Type).
  (Eq a, Arbitrary a, Show a) =>
  (a -> a -> a) ->
  a ->
  [TestTree]
absorbingLaws f absorb =
  [ testProperty "absorbtion" . property $ \x ->
      (f x absorb === absorb)
        .&&. (absorb === f absorb x)
  ]

involuteLaws ::
  forall (a :: Type).
  (Eq a, Arbitrary a, Show a) =>
  (a -> a -> a) ->
  [TestTree]
involuteLaws f =
  [ testProperty "involution" . property $ \x ->
      (f x (f x x) === x)
        .&&. (f (f x x) x === x)
  ]
