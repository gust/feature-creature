{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Documentation
( DocumentationAPI
, documentationServer
) where

import App
import Data.Int (Int64)
import Data.Text               (Text, pack)
import MimeTypes               (Markdown)
import Products.ProductsAPI    (productsAPI)
import Servant
import qualified Servant.Docs as SD

type DocumentationAPI = "docs" :> Get '[Markdown] Text

documentationServer :: App Text
documentationServer = return (pack documentation)

documentation :: String
documentation = SD.markdown (SD.docsWithIntros [intro] productsAPI)

intro :: SD.DocIntro
intro = SD.DocIntro "feature-creature" ["![](http://www.homecinemachoice.com/sites/18/images/article_images_month/2012-07/universal%20monsters%20news%2001.jpg)", "Welcome to our API", "Feel free to dig around"]

instance SD.ToSample () () where
  toSample _ = Just ()

instance SD.ToCapture (Capture "id" Int64) where
  toCapture _ = SD.DocCapture "id" "A database entity ID"
