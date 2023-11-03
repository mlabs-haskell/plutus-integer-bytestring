{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module SuitableInteger
  ( SuitableInteger,
    toInteger,
    countBytes,
  )
where

import Control.Applicative (pure)
import Control.Category ((.))
import Data.Bits (unsafeShiftL, unsafeShiftR)
import Data.Bool (otherwise)
import Data.Eq (Eq)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List (foldl')
import Data.Ord (Ord ((<)))
import Data.Semigroup ((<>))
import Data.Word (Word8)
import GHC.Integer (Integer)
import GHC.Num ((+))
import GHC.Real (fromIntegral)
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink),
    CoArbitrary (coarbitrary),
    Function (function),
    NonNegative (NonNegative),
    NonZero (NonZero),
    chooseInt,
    functionMap,
    sized,
    vectorOf,
  )
import Text.Show (Show (show))

-- Wrapper for non-negative Integers. Additionally, this uses a different
-- strategy for generation to QuickCheck's default: instead of the size
-- parameter setting the _value_, it instead sets the number of base-256 digits
-- needed to represent it (or the number of bytes its representation would
-- require if it weren't \'limbed\'), generating the number on that basis
-- instead. We also overload the Show instance to show how many bytes the
-- number's representation should minimally require for extra debugging.
--
-- We don't export the constructor to ensure that the invariants are never
-- violated: the only way to make a SuitableInteger is using the Arbitrary
-- interface.
newtype SuitableInteger = SuitableInteger (NonNegative Integer)
  deriving (Eq, Ord) via Integer

instance Show SuitableInteger where
  show (SuitableInteger (NonNegative i)) =
    show i <> " (" <> show (countBytes i) <> " bytes)"

instance Arbitrary SuitableInteger where
  arbitrary = sized $ \size ->
    SuitableInteger . NonNegative <$> do
      otherDigitCount <- chooseInt (0, size)
      -- Generate the most-significant digit first. This ensures that we don't get
      -- a leading zero, potentially truncating our result from the byte count we
      -- want.
      NonZero mostSignificantDigit <- arbitrary @(NonZero Word8)
      -- Other digits can be generated however we like.
      otherDigits <- vectorOf @Word8 otherDigitCount arbitrary
      -- To put the digits in their proper place values, we use multiplication
      -- by 256 to the power of the place needed, after an Integer conversion.
      -- Since that's equivalent to shifting by 8 times the place needed to the
      -- left (and much faster that way), this is what we use. We retain the final
      -- shift distance for the most significant digit.
      let (lastShift, otherDigitSum) = foldl' go (0, 0) otherDigits
      pure $ fromIntegral mostSignificantDigit `unsafeShiftL` lastShift + otherDigitSum
    where
      go :: (Int, Integer) -> Word8 -> (Int, Integer)
      go (shift, acc) w8 = (shift + 8, acc + fromIntegral w8 `unsafeShiftL` shift)
  shrink (SuitableInteger i) = SuitableInteger <$> shrink i

instance CoArbitrary SuitableInteger where
  coarbitrary (SuitableInteger (NonNegative i)) = coarbitrary i

instance Function SuitableInteger where
  function =
    functionMap
      (\(SuitableInteger (NonNegative i)) -> i)
      (SuitableInteger . NonNegative)

toInteger :: SuitableInteger -> Integer
toInteger (SuitableInteger (NonNegative i)) = i

-- | Counts the minimum number of bytes needed to represent an Integer.
--
-- = Preconditions
--
-- 1. @i@ is not negative
--
-- = Algorithm
--
-- 1. If @i@ is less than 256, return 1.
-- 2. Otherwise, floor divide @i@ by 256 to get @i'@, go to step 1 with @i'@,
--    then add 1 to the resulting count.
countBytes :: Integer -> Int
countBytes i
  | i < 256 = 1
  -- We use a right shift by 8 for reasons of performance: this is equivalent to
  -- a floor division by 256, but is significantly faster.
  | otherwise = 1 + countBytes (i `unsafeShiftR` 8)
