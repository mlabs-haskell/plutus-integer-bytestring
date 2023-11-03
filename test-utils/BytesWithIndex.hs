{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module BytesWithIndex
  ( BytesWithIndex,
    getBytes,
    getIndex,
  )
where

import Control.Applicative (pure, (<|>))
import Control.Category ((.))
import Control.Monad (guard)
import Data.Bool (not)
import Data.ByteString
  ( ByteString,
    cons,
    index,
    length,
    null,
  )
import Data.Eq (Eq)
import Data.Function (($))
import Data.Int (Int)
import Data.Ord ((<))
import Data.Semigroup ((<>))
import Data.Tuple (uncurry)
import Data.Word (Word8)
import GHC.Num ((-))
import Helpers (hexByte, hexByteString)
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink),
    CoArbitrary (coarbitrary),
    Function (function),
    NonNegative (NonNegative),
    chooseInt,
    functionMap,
  )
import Test.QuickCheck.Instances.ByteString ()
import Text.Show (Show (show))

-- | A non-empty 'ByteString' together with an index into that 'ByteString',
-- which is guaranteed to be in bounds. We also overload the 'Show' instance to
-- show the 'ByteString' as hex, as well as to indicate the byte at the index.
--
-- We don't export the constructor to ensure that invariants are preserved: the
-- only way to get a 'BytesWithIndex' is by using 'Arbitrary'.
data BytesWithIndex = BytesWithIndex ByteString Int
  deriving stock (Eq)

instance Show BytesWithIndex where
  show (BytesWithIndex bs i) =
    "Bytes: "
      <> hexByteString bs
      <> "\nIndex: "
      <> show i
      <> "\nByte at index: "
      <> hexByte (index bs i)

instance Arbitrary BytesWithIndex where
  arbitrary = do
    -- To ensure the ByteString is non-empty, we generate a head and a tail,
    -- then link them.
    h :: Word8 <- arbitrary
    t :: ByteString <- arbitrary
    let bs = cons h t
    i <- chooseInt (0, length bs - 1)
    pure $ BytesWithIndex bs i

  -- We have two possible shrink paths:
  --
  -- 1. Smaller index on the same ByteArray; and
  -- 2. Same or smaller index on a smaller ByteArray.
  --
  -- The second path is more aggressive (finds smaller cases faster), so we try
  -- it first.
  shrink (BytesWithIndex bs i) = shrinkPath2 <|> shrinkPath1
    where
      shrinkPath1 :: [BytesWithIndex]
      shrinkPath1 = do
        NonNegative i' <- shrink (NonNegative i)
        pure $ BytesWithIndex bs i'
      shrinkPath2 :: [BytesWithIndex]
      shrinkPath2 = do
        bs' <- shrink bs
        guard (not . null $ bs)
        NonNegative i' <- shrink (NonNegative i)
        guard (i < length bs')
        pure $ BytesWithIndex bs' i'

instance CoArbitrary BytesWithIndex where
  coarbitrary (BytesWithIndex bs i) =
    coarbitrary bs . coarbitrary i

instance Function BytesWithIndex where
  function =
    functionMap
      (\(BytesWithIndex bs i) -> (bs, i))
      (uncurry BytesWithIndex)

getBytes :: BytesWithIndex -> ByteString
getBytes (BytesWithIndex bs _) = bs

getIndex :: BytesWithIndex -> Int
getIndex (BytesWithIndex _ i) = i
