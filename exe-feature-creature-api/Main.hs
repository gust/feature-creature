{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import App
import AppConfig (AppConfig (..), getAppConfig)
import Control.Monad.Reader       (runReaderT)
import Control.Monad.Trans.Except (ExceptT)
import Config.Config (getPool)
import Data.Monoid ((<>))
import Documentation as Docs
import Products.ProductsAPI (ProductsAPI, productsServer)
import Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors
import Retry (withRetry)
import Servant
import Users.UsersAPI (UsersAPI, usersServer)

import Database.Persist.Postgresql (runSqlPool, runMigration)
import Models (migrateAll)

type FeatureCreatureAPI = "api" :> "products" :> ProductsAPI
                     :<|> "api" :> "users" :> UsersAPI
                     :<|> "api" :> Docs.DocumentationAPI

main :: IO ()
main = getAppConfig >>= \appConfig -> do
  let pool = getPool (getDBConfig appConfig)
  withRetry (runSqlPool (runMigration migrateAll) pool)

  let port = getPort appConfig
  putStrLn $ "Running web server on port:" <> (show port)
  Warp.run port (app appConfig)

app :: AppConfig -> Wai.Application
app cfg =
  (getRequestLogger cfg)
  $ cors (const $ Just corsPolicy)
  $ serve api (readerServer cfg)

api :: Proxy FeatureCreatureAPI
api = Proxy

readerServer :: AppConfig -> Server FeatureCreatureAPI
readerServer cfg =
  enter (readerToEither cfg) server

server :: ServerT FeatureCreatureAPI App
server = productsServer
    :<|> usersServer
    :<|> Docs.documentationServer

readerToEither :: AppConfig -> App :~> ExceptT ServantErr IO
readerToEither cfg =
  Nat $ \x -> runReaderT x cfg

corsPolicy :: CorsResourcePolicy
corsPolicy =
  let allowedMethods = simpleMethods <> ["DELETE", "PUT", "OPTIONS"]
      allowedHeaders = ["Content-Type"]
  in
    simpleCorsResourcePolicy { corsMethods = allowedMethods
                             , corsRequestHeaders = allowedHeaders
                             }
