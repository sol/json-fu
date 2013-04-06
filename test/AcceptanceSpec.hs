{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module AcceptanceSpec (
  main
, spec
-- exported to silence warnings
, RectangleShape(..)
, CircleShape(..)
) where

import           Test.Hspec

import           Control.Applicative
import           Data.Data
import           Data.ByteString
import           Data.JSON

main :: IO ()
main = hspec spec

data Shape = Rectangle RectangleShape | Circle CircleShape
  deriving (Eq, Show)

data RectangleShape = RectangleShape {
  rectangleShapeHeight :: Int
, rectangleShapeWidth  :: Int
} deriving (Eq, Show, Data, Typeable)

data CircleShape = CircleShape {
  circleShapeRadius :: Int
} deriving (Eq, Show, Data, Typeable)

shapeFromJSON :: ByteString -> Maybe Shape
shapeFromJSON input = do
  object <- parseJSON input
  case get "type" object of
    Just "rectangle" -> Rectangle <$> fromJSON object
    Just "circle"    -> Circle    <$> fromJSON object
    _                -> Nothing

spec :: Spec
spec = do
  feature "parsing of an object with two tagged alternatives" $ do
    scenario "parsing of first alternative" $ do
      let input = "{\"type\":\"circle\",\"radius\":23}"
      shapeFromJSON input `shouldBe` Just (Circle $ CircleShape 23)

    scenario "parsing of second alternative" $ do
      let input = "{\"type\":\"rectangle\",\"height\":23,\"width\":42}"
      shapeFromJSON input `shouldBe` Just (Rectangle $ RectangleShape 23 42)
  where
    feature = describe
    scenario = it
