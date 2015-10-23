{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module API where
  import Control.Monad.Except (runExceptT)
  import Control.Monad.IO.Class (liftIO)
  import Control.Monad.Trans.Either
  import Data.DirectoryTree
  import qualified Features.Feature as F
  import qualified Products.Product as P
  import Servant

  type ProductsAPI = "products" :> Get '[JSON] [P.Product]
                :<|> "products" :> Capture "id" P.ProductID :> "features" :> Get '[JSON] DirectoryTree

  type Handler a = EitherT ServantErr IO a

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
