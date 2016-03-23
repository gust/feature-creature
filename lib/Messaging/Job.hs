{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Messaging.Job
( Job (..)
, JobType (..)
) where

import Control.Applicative
import Data.Aeson as Aeson
import GHC.Generics (Generic)

data Job a =
  Job { getJobType :: JobType
      , getPayload :: a
      } deriving (Show, Generic)

instance ToJSON a   => ToJSON (Job a)
instance FromJSON a => FromJSON (Job a)

data JobType = ProductCreated
             | RepositoryCreated
  deriving (Show, Read, Eq)

instance ToJSON JobType where
  toJSON ProductCreated    = String "ProductCreated"
  toJSON RepositoryCreated = String "RepositoryCreated"

instance FromJSON JobType where
  parseJSON (String "ProductCreated")    = pure ProductCreated
  parseJSON (String "RepositoryCreated") = pure RepositoryCreated
  parseJSON _ = empty
