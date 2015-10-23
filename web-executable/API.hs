{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module API where
  import Control.Monad.IO.Class (liftIO)
  import Control.Monad.Trans.Either
  import qualified Database.Persist.Postgresql as DB
  import qualified Products.Product as P
  import Servant

  type ProductsAPI = "products" :> Get '[JSON] [P.Product]

  type Handler a = EitherT ServantErr IO a

  productsAPI :: Proxy ProductsAPI
  productsAPI = Proxy

  products :: Handler [P.Product]
  products = do
    prods <- liftIO P.findProducts
    return $ map DB.entityVal prods
