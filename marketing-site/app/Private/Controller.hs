module Private.Controller
  ( PrivateAPI
  , showA
  ) where

import App (AppT)
import qualified Private.View as V
import Servant
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html5 (Html)
import Users.Api

type PrivateAPI = Get '[HTML] Html

showA :: User -> AppT Html
showA user = handleShowA user

handleShowA :: User -> AppT Html
handleShowA user = return $ V.showA user
