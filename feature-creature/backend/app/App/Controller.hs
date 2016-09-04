module App.Controller
  ( AppAPI
  , showA
  ) where

import App (AppT)
import qualified App.View as V
import Servant
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html5 (Html)
import Users.Api

type AppAPI = Get '[HTML] Html

showA :: User -> AppT Html
showA user = V.showA user
