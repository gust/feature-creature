module Repositories.Controller
  ( RepositoriesAPI
  , indexA
  ) where

import App (AppT)
import Data.Aeson
  ( ToJSON
  , FromJSON
  , (.=)
  , (.:)
  )
import qualified Data.Aeson as AE
import Data.Text (Text)
import Servant
import Users.Api

type RepositoriesAPI = Get '[JSON] [Repository]

data Repository =
  Repository { getID :: Int
             , getName :: Text
             }
  deriving (Show, Eq, Ord)

instance ToJSON Repository where
  toJSON Repository{..} =
    AE.object [ "id"   .= getID
              , "name" .= getName
              ]

instance FromJSON Repository where
  parseJSON = AE.withObject "repository" $ \v -> do
    pID   <- v .:  "id"
    pName <- v .: "name"
    return (Repository pID pName)

indexA :: User -> AppT [Repository]
indexA _ = return []

