module Home.Controller
  ( HomeAPI
  , rootPath
  ) where

import App (AppT, AppConfig(..))
import Control.Monad.Reader
import Home.View (showA)
import Servant
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html5 (Html)
import Users.Api

type HomeAPI = Get '[HTML] Html

rootPath :: Maybe User -> AppT Html
rootPath user = ask >>= \AppConfig{..} ->
  return (showA user getAuthUrl)

