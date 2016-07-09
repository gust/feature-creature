{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import App
import AppConfig (AppConfig (..), getAppConfig)
import Control.Monad.Reader       (runReaderT)
import Control.Monad.Trans.Except (ExceptT)
import Data.Monoid ((<>))
import Documentation as Docs
import Features.FeaturesAPI (FeaturesAPI, featuresServer)
import Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors
import Servant

type FeaturesServiceAPI = "api" :> FeaturesAPI
                     :<|> "api" :>  Docs.DocumentationAPI

main :: IO ()
main = getAppConfig >>= \appConfig -> do
  let port = getPort appConfig
  putStrLn $ "Running web server on port:" <> (show port)
  Warp.run port (app appConfig)

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

readerToEither :: AppConfig -> App :~> ExceptT ServantErr IO
readerToEither cfg = Nat $ \x -> runReaderT x cfg

corsPolicy :: CorsResourcePolicy
corsPolicy =
  let allowedMethods = simpleMethods <> ["DELETE", "PUT", "OPTIONS"]
      allowedHeaders = ["Content-Type"]
  in
    simpleCorsResourcePolicy { corsMethods = allowedMethods
                             , corsRequestHeaders = allowedHeaders
                             }

