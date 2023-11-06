{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Control.Category ((.))
import Data.Bits (unsafeShiftL)
import Data.ByteString (ByteString, cons, replicate, snoc)
import Data.Function (($))
import Data.Functor (fmap)
import Data.Int (Int)
import Data.List (last, scanl, zip)
import Data.Semigroup ((<>))
import Data.String (String)
import Data.Tuple (snd)
import GHC.ByteOrder (ByteOrder (BigEndian, LittleEndian))
import GHC.Num (Integer, (+))
import Naive (fromByteString, toByteString)
import System.IO (IO)
import Test.Tasty.Bench
  ( Benchmark,
    bench,
    bgroup,
    defaultMain,
    nf,
  )
import Text.Show (show)

main :: IO ()
main =
  defaultMain
    [ bgroup
        "toByteString"
        [ bgroup
            "no padding"
            $$( [||
                fmap makeNoPadding noPaddingData
                  <> fmap makeNoPadding noPaddingDataLarge
                ||]
              ),
          bgroup
            "with padding"
            $$( [||
                fmap makeWithPadding padding
                  <> fmap makeWithPadding paddingLarge
                ||]
              )
        ],
      bgroup
        "fromByteString"
        [ bgroup
            "no padding"
            $$( [||
                fmap makeNoPaddingBS noPaddingBSData
                  <> fmap makeNoPaddingBS noPaddingBSDataLarge
                ||]
              ),
          bgroup
            "with padding"
            $$( [||
                fmap makeWithPaddingBS padding
                  <> fmap makeWithPaddingBS paddingLarge
                ||]
              )
        ]
    ]

-- Data sets

-- Each entry is essentially its ordinal position of 127 bytes. So we have:
--
-- 0: 127 * 256^0
-- 1: 127 * 256^1 + 127 * 256^0
-- 2: 127 * 256^2 + 127 * 256^1 + 127 * 256^0
-- ...
noPaddingData :: [(Int, Integer)]
noPaddingData = zip [1, 2 ..] $ scanl (\acc shift -> acc + 127 `unsafeShiftL` shift) 127 [8, 16 .. 56]

-- As noPaddingData, just much bigger
noPaddingDataLarge :: [(Int, Integer)]
noPaddingDataLarge =
  scanl
    (\(ix, acc) shift -> (ix + shift, acc + acc `unsafeShiftL` shift))
    (16, start)
    [16, 32, 64, 128, 256, 512]
  where
    start :: Integer
    start = snd . last $ noPaddingData

-- Same as noPaddingData, but in 'reverse'
noPaddingBSData :: [(Int, ByteString)]
noPaddingBSData = fmap (\i -> (i, replicate i 0x8F)) [1, 2 .. 8]

noPaddingBSDataLarge :: [(Int, ByteString)]
noPaddingBSDataLarge = fmap (\i -> (i, replicate i 0x8F)) [16, 32, 64, 128, 256, 512, 1024]

padding :: [Int]
padding = [0, 1 .. 7]

paddingLarge :: [Int]
paddingLarge = [15, 31, 63, 127, 255, 511, 1023]

-- Generators

makeNoPadding :: (Int, Integer) -> Benchmark
makeNoPadding (bytes, i) =
  bgroup
    groupName
    [ bench "little endian" $ nf (toByteString 0 LittleEndian) i,
      bench "big endian" $ nf (toByteString 0 BigEndian) i
    ]
  where
    groupName :: String
    groupName = show bytes <> " bytes"

makeWithPadding :: Int -> Benchmark
makeWithPadding paddingBytes =
  bgroup
    groupName
    [ bench "little endian" $ nf (toByteString (paddingBytes + 1) LittleEndian) 127,
      bench "big endian" $ nf (toByteString (paddingBytes + 1) BigEndian) 127
    ]
  where
    groupName :: String
    groupName =
      show (paddingBytes + 1)
        <> " bytes ("
        <> show paddingBytes
        <> " padding)"

makeNoPaddingBS :: (Int, ByteString) -> Benchmark
makeNoPaddingBS (bytes, bs) =
  bgroup
    groupName
    [ bench "littleEndian" $ nf (fromByteString LittleEndian) bs,
      bench "big endian" $ nf (fromByteString BigEndian) bs
    ]
  where
    groupName :: String
    groupName = show bytes <> " bytes"

makeWithPaddingBS :: Int -> Benchmark
makeWithPaddingBS paddingBytes =
  bgroup
    groupName
    [ bench "little endian" $ nf (fromByteString LittleEndian) (cons 127 . replicate paddingBytes $ 0),
      bench "big endian" $ nf (fromByteString BigEndian) (snoc (replicate paddingBytes 0) 127)
    ]
  where
    groupName :: String
    groupName =
      show (paddingBytes + 1)
        <> " bytes ("
        <> show paddingBytes
        <> " padding)"
