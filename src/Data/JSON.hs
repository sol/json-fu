module Data.JSON (
  fromJSON
, toJSON
) where

import qualified Data.JSON.Generic as G
import           Data.Data

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.ByteString.Lazy (fromChunks, toChunks)

fromJSON :: (Data a) => ByteString -> Maybe a
fromJSON = G.decode' . fromChunks . return

toJSON :: (Data a) => a -> ByteString
toJSON = B.concat . toChunks . G.encode
