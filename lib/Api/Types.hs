{-# LANGUAGE OverloadedStrings #-}

module Api.Types
( APIProduct (..)
, productToAPIProduct
) where

import Control.Monad (mzero)
import Data.Aeson
import Data.Text (Text)
import Models
import qualified Products.Product as P

data APIProduct = APIProduct { productID :: Maybe P.ProductID
                             , name      :: Text
                             , repoUrl   :: Text
                             } deriving (Show)

instance ToJSON APIProduct where
  toJSON (APIProduct prodID prodName prodRepoUrl) =
    object [ "id"      .= prodID
           , "name"    .= prodName
           , "repoUrl" .= prodRepoUrl
           ]

instance FromJSON APIProduct where
  parseJSON (Object v) = APIProduct <$>
                        v .:? "id" <*>
                        v .: "name" <*>
                        v .: "repoUrl"
  parseJSON _          = mzero

productToAPIProduct :: Maybe P.ProductID -> Product -> APIProduct
productToAPIProduct prodID p =
  APIProduct { productID = prodID
             , name      = productName p
             , repoUrl   = productRepoUrl p
             }
