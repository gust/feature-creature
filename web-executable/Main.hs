{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where
  import Documentation as Docs
  import Products.DomainTermsAPI (DomainTermsAPI, domainTermsServer)
  import Products.ProductsAPI (ProductsAPI, productsServer)
  import Network.Wai
  import Network.Wai.Handler.Warp
  import Network.Wai.Middleware.AddHeaders
  import Servant

  type FeatureCreatureAPI = ProductsAPI :<|> DomainTermsAPI :<|> Raw

  main :: IO ()
  main = run 8081 app

  server :: Server FeatureCreatureAPI
  server = productsServer
      :<|> domainTermsServer
      :<|> Docs.documentationServer

  api :: Proxy FeatureCreatureAPI
  api = Proxy

  addResponseHeaders :: Middleware
  addResponseHeaders = addHeaders [("Access-Control-Allow-Origin", "*")]

  app :: Application
  app = addResponseHeaders $ serve api server
