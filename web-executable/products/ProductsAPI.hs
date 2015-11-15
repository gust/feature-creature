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

  import Control.Monad.IO.Class (liftIO)
  import Data.Aeson
  import qualified Data.Text               as T
  import Models
  import qualified Products.DomainTermsAPI as DT
  import Products.FeaturesAPI
  import qualified Products.Product        as P
  import Servant
  import qualified Servant.Docs            as SD
  import ServantUtilities (Handler)
  import qualified Products.UserRolesAPI   as UR

  type ProductsAPI = "products" :> ProductsEndpoints
                :<|> "products" :> (
                                     ProductIDCapture :> FeaturesAPI
                                :<|> ProductIDCapture :> FeatureAPI
                                :<|> ProductIDCapture :> DT.DomainTermsAPI
                                :<|> ProductIDCapture :> DT.CreateDomainTermsAPI
                                :<|> ProductIDCapture :> UR.UserRolesAPI
                                :<|> ProductIDCapture :> UR.CreateUserRolesAPI
                                   )

  type ProductsEndpoints = Get '[JSON] [APIProduct]
  type ProductIDCapture = Capture "id" P.ProductID

  data APIProduct = APIProduct { productID :: P.ProductID
                               , name      :: T.Text
                               , repoUrl   :: T.Text
                               } deriving (Show)

  instance ToJSON APIProduct where
    toJSON (APIProduct prodID prodName prodRepoUrl) =
      object [ "id"      .= prodID
             , "name"    .= prodName
             , "repoUrl" .= prodRepoUrl
             ]

  productsServer :: Server ProductsAPI
  productsServer = products
              :<|> productsFeatures
              :<|> productsFeature
              :<|> DT.productsDomainTerms
              :<|> DT.createDomainTerm
              :<|> UR.productsUserRoles
              :<|> UR.createUserRole

  productsAPI :: Proxy ProductsAPI
  productsAPI = Proxy

  products :: Handler [APIProduct]
  products = do
    prods <- liftIO P.findProducts
    return $ map toProduct prods
      where
        toProduct dbProduct = do
          let dbProd   = P.toProduct dbProduct
          let dbProdID = P.toProductID dbProduct
          APIProduct { productID = dbProdID
                     , name      = productName dbProd
                     , repoUrl   = productRepoUrl dbProd }

  -- API Documentation Instance Definitions --

  instance SD.ToSample [APIProduct] [APIProduct] where
    toSample _ = Just $ [ sampleMonsterProduct, sampleCreatureProduct ]

  instance SD.ToCapture (Capture "id" P.ProductID) where
    toCapture _ = SD.DocCapture "id" "Product id"

  sampleMonsterProduct :: APIProduct
  sampleMonsterProduct = APIProduct { productID = 1
                                    , name      = "monsters"
                                    , repoUrl   = "http://monsters.com/repo.git"
                                    }

  sampleCreatureProduct :: APIProduct
  sampleCreatureProduct = APIProduct { productID = 2
                                     , name      = "creatures"
                                     , repoUrl   = "ssh://creatures.com/repo.git"
                                     }
