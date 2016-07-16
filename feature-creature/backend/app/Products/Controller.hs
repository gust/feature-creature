module Products.Controller
  ( ProductsAPI
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

type ProductsAPI = Get '[JSON] [Product]

data Product =
  Product { getID :: Int
          , getName :: Text
          }
  deriving (Show, Eq, Ord)

instance ToJSON Product where
  toJSON Product{..} =
    AE.object [ "id"   .= getID
              , "name" .= getName
              ]

instance FromJSON Product where
  parseJSON = AE.withObject "product" $ \v -> do
    pID   <- v .:  "id"
    pName <- v .: "name"
    return (Product pID pName)

indexA :: User -> AppT [Product]
indexA _ = return []

