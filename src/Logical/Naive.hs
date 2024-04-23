{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Logical.Naive
  ( replicate,
    and,
    or,
    xor,
    complement,
    getBit,
    setBit,
  )
where

import Control.Category ((.))
import Data.Bits (testBit, (.&.), (.|.))
import Data.Bits qualified as Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Functor.WithIndex (imap)
import GHC.Exts qualified as GHC
import Prelude
  ( Bool,
    Integer,
    Ordering (EQ, GT, LT),
    compare,
    error,
    fromInteger,
    otherwise,
    quotRem,
    take,
    ($),
    (*),
    (+),
    (-),
    (/=),
    (<),
    (>),
    (>=),
  )

replicate :: Integer -> Integer -> ByteString
replicate len byte
  | len < 0 = error "replicate: length cannot be negative"
  | byte < 0 = error "replicate: byte cannot be negative"
  | byte > 255 = error "replicate: byte too large"
  | otherwise = BS.replicate (fromInteger len) . fromInteger $ byte

and :: Integer -> ByteString -> ByteString -> ByteString
and len = case compare len 0 of
  LT -> error "and: requested a negative target length"
  EQ -> \bs -> GHC.fromList . BS.zipWith (.&.) bs
  GT -> \bs1 bs2 ->
    let lenInt = fromInteger len
        len1 = BS.length bs1
        len2 = BS.length bs2
     in if
            | len1 < lenInt -> error "and: insufficient bytes in first arg"
            | len2 < lenInt -> error "and: insufficient bytes in second arg"
            | otherwise -> GHC.fromListN lenInt . take lenInt . BS.zipWith (.&.) bs1 $ bs2

or :: Integer -> ByteString -> ByteString -> ByteString
or len = case compare len 0 of
  LT -> error "or: requested a negative target length"
  EQ -> \bs -> GHC.fromList . BS.zipWith (.|.) bs
  GT -> \bs1 bs2 ->
    let lenInt = fromInteger len
        len1 = BS.length bs1
        len2 = BS.length bs2
     in if
            | len1 < lenInt -> error "or: insufficient bytes in first arg"
            | len2 < lenInt -> error "or: insufficient bytes in second arg"
            | otherwise -> GHC.fromListN lenInt . take lenInt . BS.zipWith (.|.) bs1 $ bs2

xor :: Integer -> ByteString -> ByteString -> ByteString
xor len = case compare len 0 of
  LT -> error "xor: requested a negative target length"
  EQ -> \bs -> GHC.fromList . BS.zipWith Bits.xor bs
  GT -> \bs1 bs2 ->
    let lenInt = fromInteger len
        len1 = BS.length bs1
        len2 = BS.length bs2
     in if
            | len1 < lenInt -> error "xor: insufficient bytes in first arg"
            | len2 < lenInt -> error "xor: insufficient bytes in second arg"
            | otherwise -> GHC.fromListN lenInt . take lenInt . BS.zipWith Bits.xor bs1 $ bs2

complement :: ByteString -> ByteString
complement = BS.map Bits.complement

getBit :: ByteString -> Integer -> Bool
getBit bs ix =
  let ixInt = fromInteger ix
      len = BS.length bs
      bitLen = len * 8
   in if
          | ixInt < 0 -> error "getBit: negative index"
          | ixInt >= bitLen -> error "getBit: index out of bounds"
          | otherwise ->
              let (bigIx, littleIx) = ixInt `quotRem` 8
                  flipIx = len + bigIx - 1
               in testBit (BS.index bs flipIx) littleIx

setBit :: ByteString -> Integer -> Bool -> ByteString
setBit bs ix b =
  let ixInt = fromInteger ix
      len = BS.length bs
      bitLen = len * 8
   in if
          | ixInt < 0 -> error "setBit: negative index"
          | ixInt >= bitLen -> error "setBit: index out of bounds"
          | otherwise ->
              let (bigIx, littleIx) = ixInt `quotRem` 8
                  flipIx = len + bigIx - 1
               in GHC.fromListN len
                    . imap
                      ( \i e ->
                          if
                              | i /= flipIx -> e
                              | b -> Bits.setBit e littleIx
                              | otherwise -> Bits.clearBit e littleIx
                      )
                    . GHC.toList
                    $ bs
