{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Control.Category ((.))
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Bits (unsafeShiftL)
import Data.ByteString (ByteString, cons, replicate, snoc)
import Data.Foldable (foldl')
import Data.Function (($))
import Data.Functor (fmap)
import Data.Int (Int)
import Data.Semigroup ((<>))
import Data.String (String)
import GHC.ByteOrder (ByteOrder (BigEndian, LittleEndian))
import GHC.Num (Integer, (*), (-))
import Logical.Naive qualified as Naive
import Logical.Optimized qualified as Optimized
import Naive (fromByteString, toByteString)
import System.IO (IO)
import Test.Tasty.Bench
  ( Benchmark,
    bench,
    bgroup,
    defaultMain,
    env,
    nf,
  )
import Text.Show (show)
import Prelude (Bool (False, True), fromIntegral)

main :: IO ()
main =
  defaultMain
    [ bgroup
        "toByteString"
        [ bgroup
            "no padding"
            [ bgroup "little endian" . fmap (mkNoPaddingTBS LittleEndian) $ sizes,
              bgroup "big endian" . fmap (mkNoPaddingTBS BigEndian) $ sizes
            ],
          bgroup
            "with padding"
            [ bgroup "little endian" . fmap (mkPaddingTBS LittleEndian) $ sizes,
              bgroup "big endian" . fmap (mkPaddingTBS BigEndian) $ sizes
            ]
        ],
      bgroup
        "fromByteString"
        [ bgroup
            "no padding"
            [ bgroup "little endian" . fmap (mkNoPaddingFBS LittleEndian) $ sizes,
              bgroup "big endian" . fmap (mkNoPaddingFBS BigEndian) $ sizes
            ],
          bgroup
            "with padding"
            [ bgroup "little endian" . fmap (mkPaddingFBS LittleEndian) $ sizes,
              bgroup "big endian" . fmap (mkPaddingFBS BigEndian) $ sizes
            ]
        ],
      bgroup
        "complement"
        [ bgroup "naive" . fmap mkComplementNaive $ sizes,
          bgroup "optimized" . fmap mkComplementOptimized $ sizes
        ],
      bgroup
        "and"
        [ bgroup "naive (symmetric)" . fmap mkAndSymmetricNaive $ sizes,
          bgroup "optimized (symmetric)" . fmap mkAndSymmetricOptimized $ sizes
        ],
      bgroup
        "setBit"
        [ bgroup "naive" . fmap mkSetBitNaive $ sizes,
          bgroup "optimized" . fmap mkSetBitOptimized $ sizes
        ],
      bgroup
        "setBits"
        [ bgroup "naive" . fmap mkSetBitsNaive $ [1 .. 32],
          bgroup "optimized" . fmap mkSetBitsOptimized $ [1 .. 32]
        ],
      bgroup
        "popcount"
        [ bgroup "naive" . fmap popcountNaive $ sizes,
          bgroup "optimized" . fmap popcountOptimized $ sizes
        ],
      bgroup
        "findFirstSet"
        [ bgroup "naive (homogenous)" . fmap findFirstSetNaive $ sizes,
          bgroup "optimized (homogenous)" . fmap findFirstSetOptimized $ sizes,
          bgroup "no skip (last block)" . fmap findFirstSetNoSkip $ sizes,
          bgroup "with skip (last block)" . fmap findFirstSetWithSkip $ sizes
        ]
    ]

findFirstSetNoSkip :: Int -> Benchmark
findFirstSetNoSkip len =
  env (evaluate . force $ mkData) $ \dat ->
    bench showBytes . nf Optimized.findFirstSet $ dat
  where
    showBytes :: String
    showBytes = show len <> " bytes"
    mkData :: ByteString
    mkData = cons 0x1 . replicate (len - 1) $ 0x00

findFirstSetWithSkip :: Int -> Benchmark
findFirstSetWithSkip len =
  env (evaluate . force $ mkData) $ \dat ->
    bench showBytes . nf Optimized.findFirstSetSkip $ dat
  where
    showBytes :: String
    showBytes = show len <> " bytes"
    mkData :: ByteString
    mkData = cons 0x1 . replicate (len - 1) $ 0x00

findFirstSetNaive :: Int -> Benchmark
findFirstSetNaive len =
  env (evaluate . force $ mkData) $ \dat ->
    bench showBytes . nf Naive.findFirstSet $ dat
  where
    showBytes :: String
    showBytes = show (len - 1) <> " bytes"
    mkData :: ByteString
    mkData = replicate (len - 1) 0x00

findFirstSetOptimized :: Int -> Benchmark
findFirstSetOptimized len =
  env (evaluate . force $ mkData) $ \dat ->
    bench showBytes . nf Optimized.findFirstSet $ dat
  where
    showBytes :: String
    showBytes = show (len - 1) <> " bytes"
    mkData :: ByteString
    mkData = replicate (len - 1) 0x00

popcountNaive :: Int -> Benchmark
popcountNaive len =
  env (evaluate . force $ mkData) $ \dat ->
    bench showBytes . nf Naive.popcount $ dat
  where
    showBytes :: String
    showBytes = show (len - 1) <> " bytes"
    mkData :: ByteString
    mkData = replicate (len - 1) 0x00

popcountOptimized :: Int -> Benchmark
popcountOptimized len =
  env (evaluate . force $ mkData) $ \dat ->
    bench showBytes . nf Optimized.popcount $ dat
  where
    showBytes :: String
    showBytes = show (len - 1) <> " bytes"
    mkData :: ByteString
    mkData = replicate (len - 1) 0x00

mkSetBitsNaive :: Int -> Benchmark
mkSetBitsNaive toSet =
  env (evaluate . force $ mkData) $ \dat ->
    bench showToSet . nf (foldl' (\bs (i, b) -> Optimized.setBit bs i b) (replicate 32 0x00)) $ dat
  where
    showToSet :: String
    showToSet = show toSet <> " bits to set"
    mkData :: [(Integer, Bool)]
    mkData = fmap (,True) [0 .. fromIntegral (toSet - 1)]

mkSetBitsOptimized :: Int -> Benchmark
mkSetBitsOptimized toSet =
  env (evaluate . force $ mkData) $ \dat ->
    bench showToSet . nf (Optimized.setBits (replicate 32 0x00)) $ dat
  where
    showToSet :: String
    showToSet = show toSet <> " bits to set"
    mkData :: [(Integer, Bool)]
    mkData = fmap (,True) [0 .. fromIntegral (toSet - 1)]

mkSetBitNaive :: Int -> Benchmark
mkSetBitNaive len =
  env (evaluate . force $ mkData) $ \dat ->
    bench showBytes . nf (Naive.setBit dat 0) $ True
  where
    showBytes :: String
    showBytes = show len <> " bytes"
    mkData :: ByteString
    mkData = replicate len 0x00

mkSetBitOptimized :: Int -> Benchmark
mkSetBitOptimized len =
  env (evaluate . force $ mkData) $ \dat ->
    bench showBytes . nf (Optimized.setBit dat 0) $ True
  where
    showBytes :: String
    showBytes = show len <> " bytes"
    mkData :: ByteString
    mkData = replicate len 0x00

mkAndSymmetricNaive :: Int -> Benchmark
mkAndSymmetricNaive len =
  env (evaluate . force $ mkData) $ \dat ->
    bench showBytes . nf (Naive.and False dat) $ dat
  where
    showBytes :: String
    showBytes = show (len - 1) <> " bytes"
    mkData :: ByteString
    mkData = replicate (len - 1) 0x00

mkAndSymmetricOptimized :: Int -> Benchmark
mkAndSymmetricOptimized len =
  env (evaluate . force $ mkData) $ \dat ->
    bench showBytes . nf (Optimized.and False dat) $ dat
  where
    showBytes :: String
    showBytes = show (len - 1) <> " bytes"
    mkData :: ByteString
    mkData = replicate (len - 1) 0x00

mkComplementNaive :: Int -> Benchmark
mkComplementNaive len =
  env (evaluate . force $ mkData) $ \dat ->
    bench showBytes $ nf Naive.complement dat
  where
    showBytes :: String
    showBytes = show (len - 1) <> " bytes"
    mkData :: ByteString
    mkData = replicate (len - 1) 0x00

mkComplementOptimized :: Int -> Benchmark
mkComplementOptimized len =
  env (evaluate . force $ mkData) $ \dat ->
    bench showBytes $ nf Optimized.complement dat
  where
    showBytes :: String
    showBytes = show (len - 1) <> " bytes"
    mkData :: ByteString
    mkData = replicate (len - 1) 0x00

mkNoPaddingTBS :: ByteOrder -> Int -> Benchmark
mkNoPaddingTBS bo bytes =
  env (evaluate . force $ mkData) $ \dat ->
    bench showBytes $ nf (toByteString 0 bo) dat
  where
    showBytes :: String
    showBytes = show bytes <> " bytes"
    mkData :: Integer
    mkData = (1 `unsafeShiftL` (8 * bytes)) - 1

mkPaddingTBS :: ByteOrder -> Int -> Benchmark
mkPaddingTBS bo bytes =
  env (evaluate . force $ 127) $ \dat ->
    bench showBytes $ nf (toByteString bytes bo) dat
  where
    showBytes :: String
    showBytes = show bytes <> " bytes (" <> show (bytes - 1) <> " padding)"

mkNoPaddingFBS :: ByteOrder -> Int -> Benchmark
mkNoPaddingFBS bo bytes =
  env (evaluate . force $ mkData) $ \dat ->
    bench showBytes $ nf (fromByteString bo) dat
  where
    showBytes :: String
    showBytes = show bytes <> " bytes"
    mkData :: ByteString
    mkData = replicate bytes 0x8F

mkPaddingFBS :: ByteOrder -> Int -> Benchmark
mkPaddingFBS bo bytes =
  env (evaluate . force $ mkData) $ \dat ->
    bench showBytes $ nf (fromByteString bo) dat
  where
    showBytes :: String
    showBytes = show bytes <> " bytes (" <> show (bytes - 1) <> " padding)"
    mkData :: ByteString
    mkData = case bo of
      LittleEndian -> cons 0x8F (replicate (bytes - 1) 0x0)
      BigEndian -> snoc (replicate (bytes - 1) 0x0) 0x8F

sizes :: [Int]
sizes = [1, 2 .. 8] <> [16, 32, 64, 128, 256, 512, 1024, 2048, 4096]
