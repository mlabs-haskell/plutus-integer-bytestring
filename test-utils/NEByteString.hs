{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module NEByteString
  ( NEByteString,
    toByteString,
  )
where

import Control.Applicative (pure)
import Control.Category ((.))
import Control.Monad (guard)
import Data.Bool (not)
import Data.ByteString (ByteString, cons, null)
import Data.Eq (Eq)
import Data.Function (($))
import Data.Ord (Ord)
import Data.Word (Word8)
import Helpers (hexByteString)
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink),
    CoArbitrary (coarbitrary),
    Function (function),
    functionMap,
  )
import Test.QuickCheck.Instances.ByteString ()
import Text.Show (Show (show))

-- Wrapper for non-empty ByteStrings. We also overload the Show instance to
-- show the underlying ByteString as a list of hex bytes, rather than the
-- default, which assumes it's an ASCII string.
--
-- We don't export the constructor to ensure that the invariants are never
-- violated: the only way to make an NEByteString is using the Arbitrary
-- interface.
newtype NEByteString = NEByteString ByteString
  deriving (Eq, Ord) via ByteString

instance Show NEByteString where
  show (NEByteString bs) = hexByteString bs

instance Arbitrary NEByteString where
  arbitrary = do
    -- To ensure non-emptiness, we generate the head and tail separately
    h :: Word8 <- arbitrary
    t :: ByteString <- arbitrary
    pure . NEByteString $ cons h t
  shrink (NEByteString bs) = do
    bs' <- shrink bs
    guard (not . null $ bs)
    pure . NEByteString $ bs'

instance CoArbitrary NEByteString where
  coarbitrary (NEByteString bs) = coarbitrary bs

instance Function NEByteString where
  function = functionMap (\(NEByteString bs) -> bs) NEByteString

toByteString :: NEByteString -> ByteString
toByteString (NEByteString bs) = bs
