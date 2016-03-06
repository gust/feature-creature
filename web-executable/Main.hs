{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import App
import AppConfig (AppConfig, getAppConfig, getDBConfig)
import Control.Monad.Reader       (runReaderT)
import Control.Monad.Trans.Either (EitherT)
import Config.Config (getPool)
import Data.Monoid ((<>))
import Documentation as Docs
import Products.ProductsAPI (ProductsAPI, productsServer)
import Network.Wai as Wai
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors
import qualified Control.Retry as Retry
import Servant

import Database.Persist.Postgresql (runSqlPool, runMigration)
import Models (migrateAll)

type FeatureCreatureAPI = ProductsAPI
                     :<|> Docs.DocumentationAPI

main :: IO ()
main = do
  appConfig <- getAppConfig

  let pool = getPool (getDBConfig appConfig)

  Retry.recoverAll (Retry.constantDelay 1000000 <> Retry.limitRetries 120)
                   (\rs -> (putStrLn $ "Migrations: " ++ (show rs)) >> (runSqlPool (runMigration migrateAll) pool))

  Warp.run 8081 (app appConfig)

app :: AppConfig -> Wai.Application
app cfg =
  cors (const $ Just corsPolicy)
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
