{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module ProductsAPI where
  import Control.Monad.Except (runExceptT)
  import Control.Monad.IO.Class (liftIO)
  import Control.Monad.Trans.Either
  import Data.DirectoryTree
  import Data.Tree (Tree(Node))
  import qualified Features.Feature as F
  import qualified Products.Product as P
  import Servant
  import qualified Servant.Docs as SD


  type Handler a = EitherT ServantErr IO a

  type ProductsAPI = "products" :> Get '[JSON] [P.Product]
                :<|> "products" :> Capture "id" P.ProductID :> "features" :> Get '[JSON] DirectoryTree

  instance SD.ToSample [P.Product] [P.Product] where
    toSample _ = Just $
      [ P.Product "monsters" "http://monsters.com/repo.git"
      , P.Product "creatures" "ssh://creatures.com/repo.git"
      ]

  instance SD.ToSample DirectoryTree DirectoryTree where
    toSample _ = Just $ Node "creature" [Node "monster" []]

  instance SD.ToCapture (Capture "id" P.ProductID) where
    toCapture _ = SD.DocCapture "id" "Product id"

  productsServer :: Server ProductsAPI
  productsServer = products
              :<|> productsFeatures

  productsAPI :: Proxy ProductsAPI
  productsAPI = Proxy

  products :: Handler [P.Product]
  products = do
    prods <- liftIO P.findProducts
    return $ map P.toProduct prods

  productsFeatures :: P.ProductID -> Handler DirectoryTree
  productsFeatures prodID = do
    prodDir <- liftIO $ P.productRepositoryDir prodID
    result <- liftIO $ runExceptT (F.getFeatures prodDir)
    case result of
      Left msg -> error msg
      Right tree -> return tree
