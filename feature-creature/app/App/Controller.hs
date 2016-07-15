module App.Controller
  ( AppAPI
  , showA
  ) where

import App (AppT)
import Config.AppConfig
import Control.Monad.Reader
import qualified App.View as V
import Servant
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html5 (Html)
import Users.Api

type AppAPI = Get '[HTML] Html

showA :: User -> AppT Html
showA user = ask >>= \cfg ->
  return $ V.showA user (getAppBasePath cfg)
