{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CTConstants (bytesPerWord, checkNativeEndianness) where

import GHC.ByteOrder (ByteOrder (BigEndian, LittleEndian), targetByteOrder)
import GHC.Err (error)
import GHC.Exts (Int (I#))
import GHC.Num.WordArray (wordsToBytes#)

-- | GHC's 'GHC.Integer.Integer' uses a limbed representation, with each limb being a machine
-- word: effectively, it is a base @2^k@ system, where @k@ is the number of bits per
-- word. Since GHC allows 'GHC.Exts.Word#' to vary in width, but _bytes_ are fixed, we need
-- this constant for padding purposes.
bytesPerWord :: Int
bytesPerWord = $$([||I# (wordsToBytes# 1#)||])

-- | We only support Tier 1 platforms
-- (https://gitlab.haskell.org/ghc/ghc/-/wikis/platforms#tier-1-platforms), and
-- to ensure this, we verify, at compile time, that we have a little-endian
-- native byte order, erroring out of compilation if we don't. This allows the
-- rest of our code to safely assume we're not on a big-endian system.
--
-- We have to export this to stop GHC complaining, but it's not useful, hence
-- the type.
checkNativeEndianness :: ()
checkNativeEndianness =
  $$( [||
      case targetByteOrder of
        BigEndian -> error "Big-endian architectures are not supported."
        LittleEndian -> ()
      ||]
    )
