{-# LANGUAGE NoImplicitPrelude #-}

module Helpers (hexByte, hexByteString) where

import Control.Category ((.))
import Data.ByteString (ByteString, unpack)
import Data.Function (($))
import Data.Functor (fmap)
import Data.List (intercalate)
import Data.Semigroup ((<>))
import Data.String (String)
import Data.Word (Word8)
import Numeric (showHex)

-- Display a byte as hex
hexByte :: Word8 -> String
hexByte w8 = "0x" <> showHex w8 ""

-- Display a ByteString as a list of hex bytes
hexByteString :: ByteString -> String
hexByteString bs = "[" <> (intercalate ", " . fmap hexByte . unpack $ bs) <> "]"
