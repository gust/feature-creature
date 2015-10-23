module Main where
  import qualified API
  import Network.Wai
  import Network.Wai.Handler.Warp
  import Servant

  main :: IO ()
  main = run 8081 app

  server :: Server API.ProductsAPI
  server = API.products

  -- 'serve' comes from servant and hands you a WAI Application,
  -- which you can think of as an "abstract" web application,
  -- not yet a webserver.
  app :: Application
  app = serve API.productsAPI server
