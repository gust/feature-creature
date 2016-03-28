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
import Features.FeaturesAPI (FeaturesAPI, featuresServer)
import Network.Wai as Wai
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors
import Retry (withRetry)
import Servant

import Database.Persist.Postgresql (runSqlPool, runMigration)
import Models (migrateAll)

type FeaturesServiceAPI = FeaturesAPI
              :<|> Docs.DocumentationAPI

main :: IO ()
main = getAppConfig >>= \appConfig ->
  let pool = getPool (getDBConfig appConfig)
  in withRetry (runSqlPool (runMigration migrateAll) pool)
      >> Warp.run 8082 (app appConfig)

app :: AppConfig -> Wai.Application
app cfg =
  (getRequestLogger cfg)
  $ cors (const $ Just corsPolicy)
  $ serve api (readerServer cfg)

api :: Proxy FeaturesServiceAPI
api = Proxy

readerServer :: AppConfig -> Server FeaturesServiceAPI
readerServer cfg = enter (readerToEither cfg) server

server :: ServerT FeaturesServiceAPI App
server = featuresServer
    :<|> Docs.documentationServer

readerToEither :: AppConfig -> App :~> EitherT ServantErr IO
readerToEither cfg = Nat $ \x -> runReaderT x cfg

corsPolicy :: CorsResourcePolicy
corsPolicy =
  let allowedMethods = simpleMethods <> ["DELETE", "PUT", "OPTIONS"]
      allowedHeaders = ["Content-Type"]
  in
    simpleCorsResourcePolicy { corsMethods = allowedMethods
                             , corsRequestHeaders = allowedHeaders
                             }

