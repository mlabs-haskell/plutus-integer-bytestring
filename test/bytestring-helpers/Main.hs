{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import BSUtils
  ( cFindFirstNonZero,
    stripEndZeroes,
    stripStartZeroes,
  )
import Control.Applicative (pure)
import Control.Category ((.))
import Data.ByteString
  ( ByteString,
    cons,
    empty,
    length,
    snoc,
    useAsCStringLen,
  )
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor (fmap)
import Data.List (intercalate)
import Data.Ord (max, (<=))
import Data.Semigroup ((<>))
import Data.String (String)
import Data.Word (Word8)
import Foreign.C.Types (CSize, CUChar)
import Foreign.Ptr (Ptr, castPtr)
import GHC.Exts (toList)
import GHC.Real (fromIntegral)
import NEByteString (toByteString)
import Numeric (showHex)
import System.IO (IO)
import System.IO.Unsafe (unsafeDupablePerformIO)
import Test.QuickCheck (counterexample, property, (===))
import Test.QuickCheck.Instances.ByteString ()
import Test.Tasty
  ( TestName,
    TestTree,
    adjustOption,
    defaultMain,
    testGroup,
  )
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.QuickCheck (QuickCheckTests, testProperty)

main :: IO ()
main =
  defaultMain . adjustOption enoughTests . testGroup "ByteString helpers" $
    [ testGroup
        "stripStartZeroes"
        [ ffnzLimitProperty,
          testCase "stripStartZeroes empty = empty" $
            assertEqual "" (stripStartZeroes empty) empty,
          sszRecursiveProperty
        ],
      testGroup
        "stripEndZeroes"
        [ testCase "stripEndZeroes empty = empty" $
            assertEqual "" (stripEndZeroes empty) empty,
          sezRecursiveProperty
        ]
    ]
  where
    enoughTests :: QuickCheckTests -> QuickCheckTests
    enoughTests = max 10_000

-- Helpers

ffnzLimitProperty :: TestTree
ffnzLimitProperty = testProperty propName . property $ \neBS ->
  let input = toByteString neBS
      limit = fromIntegral $ length input
      result = unsafeDupablePerformIO $ useAsCStringLen input $ \(ptr, len) -> do
        let cucharPtr :: Ptr CUChar = castPtr ptr
        let csizeLen :: CSize = fromIntegral len
        pure $ cFindFirstNonZero cucharPtr csizeLen
   in result <= limit
  where
    propName :: TestName
    propName = "cFindFirstNonZero ptr len <= len"

sszRecursiveProperty :: TestTree
sszRecursiveProperty = testProperty propName . property $ \(bs, w8) ->
  let input = cons w8 bs
      result = stripStartZeroes input
   in counterexample ("ByteString: " <> hexByteString bs)
        . counterexample ("Byte: " <> hexByte w8)
        . counterexample ("Combined: " <> hexByteString input)
        $ if w8 == 0
          then
            let expected = stripStartZeroes bs
             in counterexample ("Expected: " <> hexByteString expected)
                  . counterexample ("Actual: " <> hexByteString result)
                  $ result === expected
          else
            counterexample ("Expected: " <> hexByteString input)
              . counterexample ("Actual: " <> hexByteString result)
              $ result === input
  where
    propName :: TestName
    propName =
      "stripStartZeroes (cons w8 bs) = "
        <> "if w8 == 0 then stripStartZeroes bs "
        <> "else cons w8 bs"

sezRecursiveProperty :: TestTree
sezRecursiveProperty = testProperty propName . property $ \(bs, w8) ->
  let input = snoc bs w8
      result = stripEndZeroes input
   in counterexample ("ByteString: " <> hexByteString bs)
        . counterexample ("Byte: " <> hexByte w8)
        $ if w8 == 0
          then
            let expected = stripEndZeroes bs
             in counterexample ("Expected: " <> hexByteString expected)
                  . counterexample ("Actual: " <> hexByteString result)
                  $ result === expected
          else
            counterexample ("Expected: " <> hexByteString input)
              . counterexample ("Actual: " <> hexByteString result)
              $ result === input
  where
    propName :: TestName
    propName =
      "stripEndZeroes (snoc bs w8) = "
        <> "if w8 == 0 then stripEndZeroes bs "
        <> "else snoc bs w8"

hexByteString :: ByteString -> String
hexByteString bs = "[" <> (intercalate ", " . fmap hexByte . toList $ bs) <> "]"

hexByte :: Word8 -> String
hexByte w8 = "0x" <> showHex w8 ""
