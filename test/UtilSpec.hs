module UtilSpec (main, spec) where

import           Test.Hspec

import           Util

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "normalizeFields" $ do
    it "converts to snake_case" $ do
      normalizeFields "Record" ["foo", "fooBar", "fooBarBaz"] `shouldBe` ["foo", "foo_bar", "foo_bar_baz"]

    context "when all fields are prefixed with constructor name" $ do
      it "strips constructor name" $ do
        normalizeFields "Record" ["recordFoo", "recordBar", "recordBaz"] `shouldBe` ["foo", "bar", "baz"]

    context "when only some fields are prefixed with constructor name" $ do
      it "does not strip constructor name" $ do
        normalizeFields "Record" ["recordFoo", "bar", "recordBaz"] `shouldBe` ["record_foo", "bar", "record_baz"]
