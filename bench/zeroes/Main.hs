{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Control.Category ((.))
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.ByteString (ByteString, replicate, singleton)
import Data.Function (($))
import Data.Functor (fmap)
import Data.Int (Int)
import Data.Semigroup ((<>))
import Data.String (String)
import GHC.ByteOrder (ByteOrder (BigEndian, LittleEndian))
import GHC.Num ((+))
import Naive (fromByteString)
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

main :: IO ()
main =
  defaultMain
    [ bgroup
        "zeroes"
        [ bgroup "little endian" . fmap (mkZeroes LittleEndian) $ sizes,
          bgroup "big endian" . fmap (mkZeroes BigEndian) $ sizes
        ]
    ]

mkZeroes :: ByteOrder -> Int -> Benchmark
mkZeroes bo zeroes =
  env (evaluate . force $ mkData) $ \dat ->
    bench showBytes $ nf (fromByteString bo) dat
  where
    showBytes :: String
    showBytes =
      show (zeroes + 2)
        <> " bytes ("
        <> show zeroes
        <> " significant zeroes)"
    mkData :: ByteString
    mkData = singleton 0xff <> replicate zeroes 0x00 <> singleton 0xff

-- Helpers

sizes :: [Int]
sizes = [1, 2 .. 5] <> [30, 62, 126, 254, 510, 1022, 2046, 4094]
