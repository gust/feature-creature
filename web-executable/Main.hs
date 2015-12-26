{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import App
import AppConfig (AppConfig, getAppConfig)
import Control.Monad.Reader       (runReaderT)
import Control.Monad.Trans.Either (EitherT)
import Documentation as Docs
import Products.ProductsAPI (ProductsAPI, productsServer)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.AddHeaders
import Servant

{- type FeatureCreatureAPI = ProductsAPI :<|> Raw -}
type FeatureCreatureAPI = ProductsAPI

main :: IO ()
main = do
  appConfig <- getAppConfig
  run 8081 (app appConfig)

readerToEither :: AppConfig -> App :~> EitherT ServantErr IO
readerToEither cfg =
  Nat $ \x -> runReaderT x cfg

readerServer :: AppConfig -> Server FeatureCreatureAPI
readerServer cfg =
  enter (readerToEither cfg) server

server :: ServerT FeatureCreatureAPI App
server =
  productsServer
  {- :<|> Docs.documentationServer -}

api :: Proxy FeatureCreatureAPI
api = Proxy

addResponseHeaders :: Middleware
addResponseHeaders =
  addHeaders [ ("Access-Control-Allow-Origin", "*")
             , ("Access-Control-Request-Method", "*")
             , ("Access-Control-Allow-Headers", "Content-Type")
             , ("Origin", "*")
             ]

{- app :: Application -}
{- app = addResponseHeaders $ serve api server -}

app :: AppConfig -> Application
app cfg =
  addResponseHeaders
  $ serve api (readerServer cfg)
