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
  import qualified Data.Text        as T
  import Models
  import qualified Products.DomainTermsAPI as DT
  import Products.FeaturesAPI
  import qualified Products.Product as P
  import Servant
  import qualified Servant.Docs     as SD
  import ServantUtilities (Handler)

  data APIProduct = APIProduct { productID :: P.ProductID
                               , name      :: T.Text
                               , repoUrl   :: T.Text
                               } deriving (Show)

  type ProductsAPI = "products" :> ProductsEndpoints
                :<|> "products" :> ProductIDCapture :> FeaturesAPI
                              :<|> ProductIDCapture :> FeatureAPI
                              :<|> ProductIDCapture :> DT.DomainTermsAPI
                              :<|> ProductIDCapture :> DT.CreateDomainTermsAPI

  type ProductIDCapture = Capture "id" P.ProductID

  type ProductsEndpoints = Get '[JSON] [APIProduct]

  instance ToJSON APIProduct where
    toJSON (APIProduct prodID prodName prodRepoUrl) =
      object [ "id"      .= prodID
             , "name"    .= prodName
             , "repoUrl" .= prodRepoUrl
             ]

  instance SD.ToSample [APIProduct] [APIProduct] where
    toSample _ = Just $
      [ APIProduct 1 "monsters" "http://monsters.com/repo.git"
      , APIProduct 2 "creatures" "ssh://creatures.com/repo.git"
      ]

  instance SD.ToCapture (Capture "id" P.ProductID) where
    toCapture _ = SD.DocCapture "id" "Product id"

  productsServer :: Server ProductsAPI
  productsServer = products
              :<|> productsFeatures
              :<|> productsFeature
              :<|> DT.productsDomainTerms
              :<|> DT.createDomainTerm

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
