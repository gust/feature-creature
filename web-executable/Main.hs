{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where
  import Documentation as Docs
  import ProductsAPI (ProductsAPI , productsServer)
  import Network.Wai
  import Network.Wai.Handler.Warp
  import Servant

  type FeatureCreatureAPI = ProductsAPI :<|> Raw

  main :: IO ()
  main = run 8081 app

  server :: Server FeatureCreatureAPI
  server = productsServer 
      :<|> Docs.documentationServer

  api :: Proxy FeatureCreatureAPI
  api = Proxy

  app :: Application
  app = serve api server
