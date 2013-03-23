module Util (normalizeFields) where

import           Data.Char
import           Data.List

normalizeFields :: String -> [String] -> [String]
normalizeFields name fields = map toSnakeCase xs
  where
    xs
      | all (uncapitalize name `isPrefixOf`) fields = map (drop (length name)) fields
      | otherwise = fields

uncapitalize :: String -> String
uncapitalize ""     = ""
uncapitalize (x:xs) = toLower x : xs

toSnakeCase :: String -> String
toSnakeCase = foldr f [] . uncapitalize
  where
    f x xs
      | isUpper x = '_' : toLower x : xs
      | otherwise      = x : xs
