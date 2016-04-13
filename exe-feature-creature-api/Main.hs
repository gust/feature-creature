{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import App
import AppConfig (AppConfig (..), getAppConfig)
import Control.Monad.Reader       (runReaderT)
import Control.Monad.Trans.Either (EitherT)
import Config.Config (getPool)
import Data.Monoid ((<>))
import Documentation as Docs
import Products.ProductsAPI (ProductsAPI, productsServer)
import Network.Wai as Wai
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors
import Retry (withRetry)
import Servant

import Database.Persist.Postgresql (runSqlPool, runMigration)
import Models (migrateAll)

type FeatureCreatureAPI = "api" :> ProductsAPI
                     :<|> "api" :> Docs.DocumentationAPI

main :: IO ()
main = getAppConfig >>= \appConfig -> do
  let pool = getPool (getDBConfig appConfig)
  withRetry (runSqlPool (runMigration migrateAll) pool)
  Warp.run 8081 (app appConfig)

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
    :<|> Docs.documentationServer

readerToEither :: AppConfig -> App :~> EitherT ServantErr IO
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
