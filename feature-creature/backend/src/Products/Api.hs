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

data Product =
  Product { getID :: Int64
          , getName :: Text
          }
  deriving (Show, Eq, Ord)

data ProductForm =
  ProductForm { getRepositoryID :: Int64
              , getRepositoryName :: Text
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
    AE.object [ "id" .= getRepositoryID
              , "name" .= getRepositoryName
              ]

instance FromJSON ProductForm where
  parseJSON = AE.withObject "productForm" $ \v -> do
    rID   <- v .:  "id"
    rName <- v .:  "name"
    return (ProductForm rID rName)

hasValidationErrors :: ProductForm -> Bool
hasValidationErrors ProductForm{..} =
  T.null getRepositoryName

formValidationErrors :: ProductForm -> Text
formValidationErrors form =
  if hasValidationErrors form then
    "Repository name required."
  else
    T.empty
