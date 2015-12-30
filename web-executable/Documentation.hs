{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Documentation
( DocumentationAPI
, documentationServer
) where

import App
import qualified Data.ByteString.Lazy   as LBS
import Data.Text               (Text, pack)
import Data.Text.Encoding      (encodeUtf8)
import Network.HTTP.Media      ((//), (/:))
import Products.ProductsAPI    (productsAPI)
import Servant
import qualified Servant.Docs as SD

type DocumentationAPI = "docs" :> Get '[Markdown] Text

data Markdown

instance Servant.Accept Markdown where
    contentType _ = "text" // "markdown" /: ("charset", "utf-8")

instance Servant.MimeRender Markdown Text where
    mimeRender _ text = LBS.fromStrict (encodeUtf8 text)

documentationServer :: App Text
documentationServer = return (pack markdown)

markdown :: String
markdown = SD.markdown (SD.docsWithIntros [intro] productsAPI)

intro :: SD.DocIntro
intro = SD.DocIntro "feature-creature" ["![](http://www.homecinemachoice.com/sites/18/images/article_images_month/2012-07/universal%20monsters%20news%2001.jpg)", "Welcome to our API", "Feel free to dig around"]
