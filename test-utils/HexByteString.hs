{-# LANGUAGE DerivingVia #-}

module HexByteString (HexByteString (HexByteString)) where

import Data.ByteString (ByteString)
import Helpers (hexByteString)
import Test.QuickCheck
  ( Arbitrary,
    CoArbitrary,
    Function (function),
    functionMap,
  )
import Test.QuickCheck.Instances.ByteString ()

-- Wrapper around ByteString with nicer display semantics.
newtype HexByteString = HexByteString ByteString
  deriving (Eq, Ord, Arbitrary, CoArbitrary) via ByteString

instance Show HexByteString where
  show (HexByteString bs) = hexByteString bs

instance Function HexByteString where
  function = functionMap (\(HexByteString bs) -> bs) HexByteString
