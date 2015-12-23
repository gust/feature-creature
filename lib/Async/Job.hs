{-# LANGUAGE DeriveGeneric #-}

module Async.Job
( Job(..)
, encodeJob
, decodeJob
) where

import Data.Aeson as Aeson
import Data.ByteString.Lazy (toStrict)
import Data.Text (Text)
import Data.Text.Lazy (fromStrict)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Lazy.Encoding as TLE
import GHC.Generics (Generic)

data Job a =
  Job { jobType :: Text
      , payload :: a
      } deriving (Show, Generic)

instance ToJSON a   => ToJSON (Job a)
instance FromJSON a => FromJSON (Job a)

encodeJob :: ToJSON a => Job a -> Text
encodeJob = decodeUtf8 . toStrict . Aeson.encode

decodeJob :: FromJSON a => Text -> Maybe (Job a)
decodeJob = Aeson.decode . TLE.encodeUtf8 . fromStrict
