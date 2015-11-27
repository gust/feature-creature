{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where
  import Documentation as Docs
  import Products.ProductsAPI (ProductsAPI, productsServer)
  import Network.Wai
  import Network.Wai.Handler.Warp
  import Network.Wai.Middleware.AddHeaders
  import Servant

  type FeatureCreatureAPI = ProductsAPI :<|> Raw

  main :: IO ()
  main = run 8081 app

  server :: Server FeatureCreatureAPI
  server = productsServer
      :<|> Docs.documentationServer

  api :: Proxy FeatureCreatureAPI
  api = Proxy

  addResponseHeaders :: Middleware
  addResponseHeaders = addHeaders [ ("Access-Control-Allow-Origin", "*")
                                  , ("Access-Control-Request-Method", "*")
                                  , ("Access-Control-Allow-Headers", "Content-Type")
                                  , ("Origin", "*")
                                  ]

  app :: Application
  app = addResponseHeaders $ serve api server
