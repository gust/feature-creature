module Products.Controller
  ( ProductsAPI
  , actions
  , indexA
  , createA
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
import Errors
import Servant
import Users.Api

type ProductsAPI = Get '[JSON] [Product]
              :<|> ReqBody '[JSON] ProductForm :> Post '[JSON] Product

actions :: User -> ServerT ProductsAPI AppT
actions user = indexA user :<|> createA user

data Product =
  Product { getID :: Int
          , getName :: Text
          }
  deriving (Show, Eq, Ord)

data ProductForm =
  ProductForm { getRepositoryID :: Int
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


instance ToJSON ProductForm where
  toJSON ProductForm{..} =
    AE.object [ "id" .= getRepositoryID ]

instance FromJSON ProductForm where
  parseJSON = AE.withObject "productForm" $ \v -> do
    rID <- v .:  "id"
    return (ProductForm rID)

indexA :: User -> AppT [Product]
indexA _ = return []

createA :: User -> ProductForm -> AppT Product
createA _ _ = raiseAppError (ServerError $ Just "I'm not implemented yet!")
