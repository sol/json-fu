{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Data.JSONSpec where

import           Test.Hspec

import           Control.Monad
import           Data.ByteString.Char8 (ByteString)
import           Data.Data
import qualified Data.Set as Set
import           Data.JSON

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
  describe "toJSON" $ do
    it "encodes Int-values as numbers" $ do
      toJSON (Just (23 :: Int)) `shouldBe` "23"

    it "encodes lists as lists" $ do
      toJSON [23, 42 :: Int] `shouldBe` "[23,42]"

    it "encodes sets as lists" $ do
      toJSON (Set.fromList [23, 42 :: Int]) `shouldBe` "[23,42]"

    context "when encoding Maybe" $ do
      it "encodes Just the same way as the inner type" $ do
        toJSON (Just 23 :: Maybe Int) `shouldBe` "23"

      it "encodes Nothing as null" $ do
        toJSON (Nothing :: Maybe Int) `shouldBe` "null"

    context "when encoding ADTs" $ do
      it "encodes unit types as empty lists" $ do
        toJSON Unit `shouldBe` "[]"

      it "encodes newtype-wrapped types the same way as the inner type" $ do
        toJSON (Newtype 23) `shouldBe` "23"

      it "encodes wrapped types the same way as the inner type" $ do
        toJSON (Wrapped 23) `shouldBe` "23"

      it "encodes products as lists" $ do
        toJSON (Product "test" 23) `shouldBe` "[\"test\",23]"

      it "encodes sums as strings" $ do
        toJSON SumA `shouldBe` "\"SumA\""

      context "when encoding sums of products" $ do

        context "when encoding constructors with no arguments" $ do
          it "uses strings" $ do
            toJSON SumOfProductsC `shouldBe` "\"SumOfProductsC\""

        context "when encoding constructors with one argument" $ do
          it "uses objects with a value field" $ do
            toJSON (SumOfProductsB 23) `shouldBe` "{\"SumOfProductsB\":23}"

        context "when encoding constructors with several arguments" $ do
          it "uses objects with a list field" $ do
            toJSON (SumOfProductsA "test" 23) `shouldBe` "{\"SumOfProductsA\":[\"test\",23]}"

      it "encodes records as objects" $ do
        toJSON (Record "test" 23) `shouldBe` "{\"field_bar\":23,\"field_foo\":\"test\"}"

      it "encodes record-style sum types as nested objects" $ do
        toJSON (RecordSumA "test" 23) `shouldBe` "{\"RecordSumA\":{\"record_sum2\":23,\"record_sum1\":\"test\"}}"

  describe "fromJSON'" $ do
    let decode :: Data a => ByteString -> Maybe a
        decode = parseJSON >=> fromJSON
    context "when decoding ADTs" $ do
      it "decodes records from objects" $ do
        decode "{\"field_foo\":\"test\",\"field_bar\":23}" `shouldBe` Just (Record "test" 23)

      it "decodes record-style sum types from nested objects" $ do
        decode "{\"RecordSumA\":{\"record_sum2\":23,\"record_sum1\":\"test\"}}" `shouldBe` Just (RecordSumA "test" 23)
