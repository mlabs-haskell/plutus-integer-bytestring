{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Control.Category ((.))
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Bits (unsafeShiftL)
import Data.ByteString (ByteString, cons, replicate, snoc)
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
import Prelude (Bool (False))

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
        ]
    ]

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
