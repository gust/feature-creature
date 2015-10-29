{-# LANGUAGE OverloadedStrings #-}

module Documentation where
  import Data.ByteString.Lazy         (ByteString)
  import Data.Text.Lazy               (pack)
  import Data.Text.Lazy.Encoding      (encodeUtf8)
  import Network.HTTP.Types           (ok200)
  import Network.Wai                  (responseLBS)
  import ProductsAPI                  (productsAPI)
  import qualified Servant.Docs as SD

  -- not sure of the type here. snippet taken from: http://haskell-servant.github.io/tutorial/docs.html
  documentationServer _ respond = respond $ responseLBS ok200 [plain] docsBS
    where
      plain       = ("Content-Type", "text/plain")

  docsBS :: ByteString
  docsBS = encodeUtf8
         . pack
         . SD.markdown
         $ SD.docsWithIntros [intro] productsAPI
    where
      intro = SD.DocIntro "feature-creature" ["![](http://www.homecinemachoice.com/sites/18/images/article_images_month/2012-07/universal%20monsters%20news%2001.jpg)", "Welcome to our API", "Feel free to dig around"]
