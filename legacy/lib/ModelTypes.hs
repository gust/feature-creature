{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module ModelTypes
( RepositoryState (..)
) where

import qualified Data.Aeson as AE
import Control.Applicative
import Database.Persist.TH (derivePersistField)

data RepositoryState = Unready
                     | Ready
                     | Error
  deriving (Show, Read, Eq)

instance AE.ToJSON RepositoryState where
  toJSON Unready = AE.String "Unready"
  toJSON Ready   = AE.String "Ready"
  toJSON Error   = AE.String "Error"

instance AE.FromJSON RepositoryState where
  parseJSON (AE.String "Unready") = pure Unready
  parseJSON (AE.String "Ready")   = pure Ready
  parseJSON (AE.String "Error")   = pure Error
  parseJSON _ = empty

derivePersistField "RepositoryState"
