{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module ProductsAPI
  ( ProductsAPI
  , productsAPI
  , productsServer
  ) where

  import Control.Monad.Except (runExceptT)
  import Control.Monad.IO.Class (liftIO)
  import Control.Monad.Trans.Either
  import Data.Aeson
  import Data.DirectoryTree
  import qualified Data.Text        as T
  import Data.Tree (Tree(Node))
  import qualified Features.Feature as F
  import qualified Products.Product as P
  import Servant
  import qualified Servant.Docs     as SD
  import Models

  data APIProduct = APIProduct { productID :: P.ProductID
                               , name      :: T.Text
                               , repoUrl   :: T.Text 
                               } deriving (Show)

  type Handler a           = EitherT ServantErr IO a

  type ProductsAPI = "products" :> ( Get '[JSON] [APIProduct]
                                :<|> Capture "id" P.ProductID :> FeaturesAPI
                                   )
  type FeaturesAPI = "features" :> Get '[JSON] DirectoryTree

  instance ToJSON APIProduct where
    toJSON (APIProduct prodID prodName prodRepoUrl) = object [
      "id" .= prodID
      , "name" .= prodName
      , "repoUrl" .= prodRepoUrl
      ]

  instance SD.ToSample [APIProduct] [APIProduct] where
    toSample _ = Just $
      [ APIProduct 1 "monsters" "http://monsters.com/repo.git"
      , APIProduct 2 "creatures" "ssh://creatures.com/repo.git"
      ]

  instance SD.ToSample DirectoryTree DirectoryTree where
    toSample _ = Just featureDirectoryExample

  instance SD.ToCapture (Capture "id" P.ProductID) where
    toCapture _ = SD.DocCapture "id" "Product id"

  productsServer :: Server ProductsAPI
  productsServer = products
              :<|> productsFeatures

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

  productsFeatures :: P.ProductID -> Handler DirectoryTree
  productsFeatures prodID = do
    prodDir <- liftIO $ P.productRepositoryDir prodID
    result <- liftIO $ runExceptT (F.getFeatures prodDir)
    case result of
      Left msg -> error msg
      Right tree -> return tree

  featureDirectoryExample :: DirectoryTree
  featureDirectoryExample = rootNode
    where
      rootNode = Node "features" [creatures]
      creatures = Node "creatures" [swampThing, wolfman]
      swampThing = Node "swamp-thing" [
        Node "vegetable-mind-control.feature" [],
        Node "limb-regeneration.feature" []
        ]
      wolfman = Node "wolfman" [
        Node "shape-shifting.feature" [],
        Node "animal-instincts.feature" []
        ]
