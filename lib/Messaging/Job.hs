{-# LANGUAGE DeriveGeneric #-}

module Messaging.Job
( Job(..)
) where

import Data.Aeson as Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

data Job a =
  Job { getJobType :: Text
      , getPayload :: a
      } deriving (Show, Generic)

instance ToJSON a   => ToJSON (Job a)
instance FromJSON a => FromJSON (Job a)

