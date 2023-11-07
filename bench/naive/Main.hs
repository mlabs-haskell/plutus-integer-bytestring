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

-- = Note on our use of (typed) TH
--
-- We use typed TH to programmatically define our benches. The reason we use
-- this method stems fundamentally from the issue described here:
-- https://github.com/Bodigrim/tasty-bench#isolating-interfering-benchmarks . In
-- short, we have non-trivially sized data, which we have to ensure doesn't
-- interfere with our measurements.
--
-- This basically gives us three possibilities:
--
-- 1. Use manually-defined (and very repetitive) benchmarks.
-- 2. Use the fix suggested in the link.
-- 3. Use typed TH.
--
-- 1 is not an adequate solution: it is tedious, brittle, repetitive and
-- non-extensible. 2 will not work, both env and withResource require a
-- compile-time-known test structure, which (although _technically_ the case for
-- us) GHC and Tasty cannot prove, which produces an error instead of running.
-- Thus, the only remaining option is 3, which effectively acts to convince both
-- GHC and Tasty that our benchmarking structure truly is static.
--
-- = Note on benchmarking data
--
-- Broadly, we want our benchmarks to answer the following questions:
--
-- 1. Does (requested or specified) byte order affect performance, all other
--    things being equal?
-- 2. How does more versus less padding affect performance?
-- 3. Is a conversion in one direction costlier than in the other, assuming
--    equivalent representations?
-- 4. How do the operations scale with larger inputs?
--
-- With these questions in mind, we decided to base our measurements on the byte
-- size of what is going to be operated over. For toByteString, this will be the
-- size of the resulting ByteString; for fromByteString, this will be the size
-- of the input. We use two sets of sizes: a 'small' and a 'large' set. The
-- 'small' set consists of sizes 1 through 8, while the 'large' set consists of
-- sizes 16, 32, 64, 128, 256, 512 and 1024.We choose this separation to make
-- the data easier to read, but also to give us a spread between 'small' inputs
-- (which we regard as more typical) and 'large' inputs (which can come up, but
-- less frequently). We use this distinction to help answer question 4.
--
-- To answer question 2, we test our operations in two different ways:
--
-- - Minimal padding (that is, every byte is significant); and
-- - Maximal padding (that is, only one byte is significant).
--
-- This isolates the cost of padding from the cost of conversion.
--
-- The actual benchmarks consist of the following:
--
-- 1. toByteString, no padding, both endiannesses.
-- 2. toByteString, maximal padding, both endiannesses.
-- 3. fromByteString, no padding, both endiannesses.
-- 4. fromByteString, maximal padding, both endiannesses.
--
-- By varying the endiannesses in each benchmark, keeping all else constant, we
-- can answer question 1; by trying all sizes for both operations, we can answer
-- question 3.
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
