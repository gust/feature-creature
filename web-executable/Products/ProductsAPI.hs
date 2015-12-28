{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Products.ProductsAPI
( ProductsAPI
, productsAPI
, productsServer
) where

import App
import AppConfig (getDBConfig, getGitConfig)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader
import Control.Monad.Trans.Either (left)
import Data.Aeson
import qualified Data.Text               as T
import qualified Data.ByteString.Lazy.Char8 as BS
import Models
import qualified Products.DomainTermsAPI as DT
import qualified Products.FeaturesAPI    as F
import qualified Products.Product        as P
import Servant
import qualified Servant.Docs            as SD
import qualified Products.UserRolesAPI   as UR

type ProductsAPI = "products" :> Get '[JSON] [APIProduct]
              :<|> "products" :> ReqBody '[JSON] APIProduct :> Post '[JSON] APIProduct
              :<|> "products" :> ProductIDCapture :> F.FeaturesAPI
              :<|> "products" :> ProductIDCapture :> F.FeatureAPI
              :<|> "products" :> ProductIDCapture :> DT.DomainTermsAPI
              :<|> "products" :> ProductIDCapture :> DT.CreateDomainTermsAPI
              :<|> "products" :> ProductIDCapture :> UR.UserRolesAPI
              :<|> "products" :> ProductIDCapture :> UR.CreateUserRolesAPI

type ProductIDCapture = Capture "id" P.ProductID

data APIProduct = APIProduct { productID :: Maybe P.ProductID
                             , name      :: T.Text
                             , repoUrl   :: T.Text
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

productsServer :: ServerT ProductsAPI App
productsServer = products
            :<|> createProduct
            :<|> F.productsFeatures
            :<|> F.productsFeature
            :<|> DT.productsDomainTerms
            :<|> DT.createDomainTerm
            :<|> UR.productsUserRoles
            :<|> UR.createUserRole

productsAPI :: Proxy ProductsAPI
productsAPI = Proxy

createProduct :: APIProduct -> App APIProduct
createProduct (APIProduct _ prodName prodRepoUrl) = do
  let newProduct = P.Product prodName prodRepoUrl
  dbConfig <- reader getDBConfig
  gitConfig <- reader getGitConfig
  result <- liftIO $ runExceptT $ P.createProduct dbConfig gitConfig newProduct
  case result of
    Left err ->
      -- In the case where the repo cannot be retrieved,
      -- It's probably a good idea to rollback the Product creation here.
      lift $ left $ err503 { errBody = BS.pack err }
    Right prodID ->
      return $ APIProduct { productID = Just prodID
                          , name      = prodName
                          , repoUrl   = prodRepoUrl
                          }

products :: App [APIProduct]
products = do
  dbConfig <- reader getDBConfig
  prods <- liftIO $ P.findProducts dbConfig
  return $ map toProduct prods
    where
      toProduct dbProduct = do
        let dbProd   = P.toProduct dbProduct
        let dbProdID = P.toProductID dbProduct
        APIProduct { productID = Just dbProdID
                   , name      = productName dbProd
                   , repoUrl   = productRepoUrl dbProd }

-- API Documentation Instance Definitions --

instance SD.ToSample [APIProduct] [APIProduct] where
  toSample _ = Just $ [ sampleMonsterProduct, sampleCreatureProduct ]

instance SD.ToSample APIProduct APIProduct where
  toSample _ = Just sampleCreatureProduct

instance SD.ToCapture (Capture "id" P.ProductID) where
  toCapture _ = SD.DocCapture "id" "Product id"

sampleMonsterProduct :: APIProduct
sampleMonsterProduct = APIProduct { productID = Just 1
                                  , name      = "monsters"
                                  , repoUrl   = "http://monsters.com/repo.git"
                                  }

sampleCreatureProduct :: APIProduct
sampleCreatureProduct = APIProduct { productID = Just 2
                                   , name      = "creatures"
                                   , repoUrl   = "ssh://creatures.com/repo.git"
                                   }
