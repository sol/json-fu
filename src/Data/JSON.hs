module Data.JSON (
  fromJSON
, toJSON
, parseJSON
, get
) where

import           Data.Data
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as H
import           Data.Text (Text)
import           Data.Attoparsec (parse, maybeResult)
import           Data.Aeson (Value(..), Result(..), json')
import qualified Data.Aeson.Encode as E

import qualified Data.JSON.Generic as G

parseJSON :: ByteString -> Maybe Value
parseJSON = maybeResult . parse json'

fromJSON :: Data a => Value -> Maybe a
fromJSON v = case G.fromJSON v of
  Success a -> Just a
  Error _   -> Nothing

toJSON :: (Data a) => a -> ByteString
toJSON = B.concat . L.toChunks . E.encode . G.toJSON

get :: Text -> Value -> Maybe Value
get k v = case v of
  Object o -> H.lookup k o
  _ -> Nothing
