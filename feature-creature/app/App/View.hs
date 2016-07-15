{-# LANGUAGE QuasiQuotes #-}

module App.View
  ( showA
  ) where

import App (AppT)
import Config.AppConfig
import Control.Monad.Reader
import Layouts.Default (withDefaultLayout)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.QuasiText
import Users.Api (User (..))

showA :: User -> AppT Html
showA user = ask >>= \AppConfig{..} -> return $ withDefaultLayout $ do
  let userName = name user
  H.div ! A.id "app" $ H.script $ H.text $
    [embed|
      init('$getEnv', '$getAppBasePath', '$userName');
    |]
