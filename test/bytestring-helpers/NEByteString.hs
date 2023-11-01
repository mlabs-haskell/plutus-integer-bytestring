{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}

module NEByteString
  ( -- * Types
    NEByteString,

    -- * Functions
    toByteString,
    uncons,
  )
where

import Control.Applicative ((<*>))
import Control.Category ((.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe (unsafeHead, unsafeTail)
import Data.Eq (Eq)
import Data.Function (($))
import Data.Functor (fmap, (<$>))
import Data.List (intercalate)
import Data.Maybe (Maybe (Just, Nothing))
import Data.Semigroup ((<>))
import Data.String (String)
import Data.Word (Word8)
import Numeric (showHex)
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink),
    CoArbitrary (coarbitrary),
    Function (function),
    functionMap,
  )
import Test.QuickCheck.Instances.ByteString ()
import Text.Show (Show (show))

-- | A known non-empty 'ByteString', for use with QuickCheck properties that
-- require such. We also modify its 'Show' instance to display it as a list of
-- hex bytes, rather than the default, which is an ASCII string.
--
-- We don't export the constructor, instead providing two \'unpacking\'
-- functions, to ensure that the only way you can get hold of a 'NEByteString'
-- is via 'arbitrary', 'shrink' or something similar, guaranteeing its
-- invariants hold.
newtype NEByteString = NEByteString ByteString
  deriving (Eq) via ByteString

instance Show NEByteString where
  show (NEByteString bs) = "[" <> (intercalate ", " . fmap hexByte . BS.unpack $ bs) <> "]"
    where
      hexByte :: Word8 -> String
      hexByte w8 = "0x" <> showHex w8 ""

instance Arbitrary NEByteString where
  arbitrary = NEByteString <$> (BS.cons <$> arbitrary <*> arbitrary)
  shrink (NEByteString bs) =
    NEByteString <$> do
      let h = unsafeHead bs
      let t = unsafeTail bs
      case BS.uncons t of
        Nothing -> BS.singleton <$> shrink h
        Just _ -> BS.cons <$> shrink h <*> shrink t

instance CoArbitrary NEByteString where
  coarbitrary (NEByteString bs) = coarbitrary bs

instance Function NEByteString where
  function = functionMap toByteString NEByteString

-- | \'Unpack\', forgetting invariants.
toByteString :: NEByteString -> ByteString
toByteString (NEByteString bs) = bs

-- | \'Unpack\' into head and tail, without partiality.
uncons :: NEByteString -> (Word8, ByteString)
uncons (NEByteString bs) = (unsafeHead bs, unsafeTail bs)
