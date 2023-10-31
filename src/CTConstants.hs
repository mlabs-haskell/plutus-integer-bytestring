{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CTConstants (bytesPerWord) where

import GHC.Exts (Int (I#))
import GHC.Num.WordArray (wordsToBytes#)

-- GHC's Integer uses a limbed representation, with each limb being a machine
-- word: effectively, it is a base 2^k system, where k is the number of bits per
-- word. Since GHC allows Word# to vary in width, but _bytes_ are fixed, we need
-- this constant for padding purposes.
bytesPerWord :: Int
bytesPerWord = $$([||I# (wordsToBytes# 1#)||])
