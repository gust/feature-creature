module Products.Api
  ( Product (..)
  , ProductForm (..)
  , hasValidationErrors
  , formValidationErrors
  ) where

import Data.Aeson
  ( ToJSON
  , FromJSON
  , (.=)
  , (.:)
  )
import qualified Data.Aeson as AE
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Repositories

data Product =
  Product { getID :: Int64
          , getName :: Text
          }
  deriving (Show, Eq, Ord)

data ProductForm = ProductForm { getRepositoryForm :: RepositoryForm }
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

instance FromJSON ProductForm where
  parseJSON = AE.withObject "productForm" $ \v -> do
    r <- v .:  "repositoryForm"
    return (ProductForm r)

hasValidationErrors :: ProductForm -> Bool
hasValidationErrors _ = False -- TODO: validate form

formValidationErrors :: ProductForm -> Text
formValidationErrors _ = T.empty
