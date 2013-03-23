{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Data.JSON.GenericSpec where

import           Test.Hspec

import           Data.ByteString.Lazy.Char8 ()
import           Data.Data
import qualified Data.Set as Set
import           Data.JSON.Generic

main :: IO ()
main = hspec spec

data Unit = Unit
  deriving (Eq, Show, Data, Typeable)

data Product = Product String Int
  deriving (Eq, Show, Data, Typeable)

data Sum = SumA | SumB | SumC
  deriving (Eq, Show, Data, Typeable)

data SumOfProducts = SumOfProductsA String Int | SumOfProductsB Int | SumOfProductsC
  deriving (Eq, Show, Data, Typeable)

data Record = Record {
  recordFieldFoo :: String
, recordFieldBar :: Int
} deriving (Eq, Show, Data, Typeable)

data RecordSum
  = RecordSumA {recordSum1 :: String , recordSum2 :: Int}
  | RecordSumB {recordSum1 :: String , recordSum2 :: Int}
  deriving (Eq, Show, Data, Typeable)

newtype Newtype = Newtype Int
  deriving (Eq, Show, Data, Typeable)

data Wrapped = Wrapped Int
  deriving (Eq, Show, Data, Typeable)

spec :: Spec
spec = do
  describe "encode" $ do
    it "encodes Int-values as numbers" $ do
      encode (Just (23 :: Int)) `shouldBe` "23"

    it "encodes lists as lists" $ do
      encode [23, 42 :: Int] `shouldBe` "[23,42]"

    it "encodes sets as lists" $ do
      encode (Set.fromList [23, 42 :: Int]) `shouldBe` "[23,42]"

    context "when encoding Maybe" $ do
      it "encodes Just the same way as the inner type" $ do
        encode (Just 23 :: Maybe Int) `shouldBe` "23"

      it "encodes Nothing as null" $ do
        encode (Nothing :: Maybe Int) `shouldBe` "null"

    context "when encoding ADTs" $ do
      it "encodes unit types as empty lists" $ do
        encode Unit `shouldBe` "[]"

      it "encodes newtype-wrapped types the same way as the inner type" $ do
        encode (Newtype 23) `shouldBe` "23"

      it "encodes wrapped types the same way as the inner type" $ do
        encode (Wrapped 23) `shouldBe` "23"

      it "encodes products as lists" $ do
        encode (Product "test" 23) `shouldBe` "[\"test\",23]"

      it "encodes sums as strings" $ do
        encode SumA `shouldBe` "\"SumA\""

      context "when encoding sums of products" $ do

        context "when encoding constructors with no arguments" $ do
          it "uses strings" $ do
            encode SumOfProductsC `shouldBe` "\"SumOfProductsC\""

        context "when encoding constructors with one argument" $ do
          it "uses objects with a value field" $ do
            encode (SumOfProductsB 23) `shouldBe` "{\"SumOfProductsB\":23}"

        context "when encoding constructors with several arguments" $ do
          it "uses objects with a list field" $ do
            encode (SumOfProductsA "test" 23) `shouldBe` "{\"SumOfProductsA\":[\"test\",23]}"

      it "encodes records as objects" $ do
        encode (Record "test" 23) `shouldBe` "{\"field_foo\":\"test\",\"field_bar\":23}"

      it "encodes record-style sum types as nested objects" $ do
        encode (RecordSumA "test" 23) `shouldBe` "{\"RecordSumA\":{\"record_sum2\":23,\"record_sum1\":\"test\"}}"

  describe "decode'" $ do
    context "when decoding ADTs" $ do
      it "decodes records from objects" $ do
        decode' "{\"field_foo\":\"test\",\"field_bar\":23}" `shouldBe` Just (Record "test" 23)

      it "decodes record-style sum types from nested objects" $ do
        decode' "{\"RecordSumA\":{\"record_sum2\":23,\"record_sum1\":\"test\"}}" `shouldBe` Just (RecordSumA "test" 23)
