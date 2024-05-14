{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
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
    setBits,
    popcount,
    findFirstSet,
    rotate,
    shift,
  )
where

import Control.Category ((.))
import Data.Bits (testBit, (.&.), (.|.))
import Data.Bits qualified as Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (foldl')
import Data.Functor.WithIndex (imap)
import Data.Maybe (mapMaybe)
import GHC.Exts qualified as GHC
import Prelude
  ( Bool (True),
    Int,
    Integer,
    Maybe (Nothing),
    abs,
    error,
    fromInteger,
    fromIntegral,
    min,
    mod,
    not,
    otherwise,
    pure,
    quotRem,
    rem,
    signum,
    ($),
    (*),
    (+),
    (-),
    (/=),
    (<),
    (<>),
    (==),
    (>),
    (>=),
  )

replicate :: Integer -> Integer -> ByteString
replicate len byte
  | len < 0 = error "replicate: length cannot be negative"
  | byte < 0 = error "replicate: byte cannot be negative"
  | byte > 255 = error "replicate: byte too large"
  | otherwise = BS.replicate (fromInteger len) . fromInteger $ byte

and :: Bool -> ByteString -> ByteString -> ByteString
and shouldPad bs1 bs2 =
  let len1 = BS.length bs1
      len2 = BS.length bs2
      lenCombined = min len1 len2
      combined = GHC.fromListN lenCombined . BS.zipWith (.&.) bs1 $ bs2
   in if
          | not shouldPad -> combined
          | len1 > lenCombined -> combined <> BS.drop lenCombined bs1
          | len2 > lenCombined -> combined <> BS.drop lenCombined bs2
          | otherwise -> combined

or :: Bool -> ByteString -> ByteString -> ByteString
or shouldPad bs1 bs2 =
  let len1 = BS.length bs1
      len2 = BS.length bs2
      lenCombined = min len1 len2
      combined = GHC.fromListN lenCombined . BS.zipWith (.|.) bs1 $ bs2
   in if
          | not shouldPad -> combined
          | len1 > lenCombined -> combined <> BS.drop lenCombined bs1
          | len2 > lenCombined -> combined <> BS.drop lenCombined bs2
          | otherwise -> combined

xor :: Bool -> ByteString -> ByteString -> ByteString
xor shouldPad bs1 bs2 =
  let len1 = BS.length bs1
      len2 = BS.length bs2
      lenCombined = min len1 len2
      combined = GHC.fromListN lenCombined . BS.zipWith Bits.xor bs1 $ bs2
   in if
          | not shouldPad -> combined
          | len1 > lenCombined -> combined <> BS.drop lenCombined bs1
          | len2 > lenCombined -> combined <> BS.drop lenCombined bs2
          | otherwise -> combined

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
                  flipIx = len - bigIx - 1
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
                  flipIx = len - bigIx - 1
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

setBits :: ByteString -> [(Integer, Bool)] -> ByteString
setBits = foldl' (\bs (i, b) -> setBit bs i b)

popcount :: ByteString -> Int
popcount bs = foldl' go 0 [0 .. bitLen - 1]
  where
    bitLen :: Int
    bitLen = BS.length bs * 8
    go :: Int -> Int -> Int
    go acc ix =
      if getBit bs (fromIntegral ix)
        then acc + 1
        else acc

findFirstSet :: ByteString -> Int
findFirstSet bs = go [0 .. bitLen - 1]
  where
    bitLen :: Int
    bitLen = BS.length bs * 8
    go :: [Int] -> Int
    go = \case
      [] -> (-1)
      (ix : ixes) ->
        if getBit bs (fromIntegral ix)
          then ix
          else go ixes

shift :: ByteString -> Int -> ByteString
shift bs bitMove
  | BS.null bs = bs
  | bitMove == 0 = bs
  | otherwise =
      let start = replicate (fromIntegral len) 0x0
       in if abs bitMove >= bitLen
            then start
            else
              let changelist = case signum bitMove of
                    (-1) -> mapMaybe (go bitMove) [abs bitMove .. bitLen - 1]
                    _ -> mapMaybe (go bitMove) [0 .. bitLen - bitMove - 1]
               in setBits start changelist
  where
    len :: Int
    len = BS.length bs
    bitLen :: Int
    bitLen = len * 8
    go :: Int -> Int -> Maybe (Integer, Bool)
    go move oldIx =
      if getBit bs . fromIntegral $ oldIx
        then pure (fromIntegral $ oldIx + move, True)
        else Nothing

rotate :: ByteString -> Int -> ByteString
rotate bs bitMove
  | BS.null bs = bs
  | otherwise = case bitMove `mod` bitLen of
      0 -> bs
      reducedMove ->
        let start = replicate (fromIntegral len) 0x0
            changelist = mapMaybe (go reducedMove) [0 .. bitLen - 1]
         in setBits start changelist
  where
    len :: Int
    len = BS.length bs
    bitLen :: Int
    bitLen = len * 8
    go :: Int -> Int -> Maybe (Integer, Bool)
    go move oldIx =
      if getBit bs . fromIntegral $ oldIx
        then pure (fromIntegral ((oldIx + bitLen + move) `rem` bitLen), True)
        else Nothing
