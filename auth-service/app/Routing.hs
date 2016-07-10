module Routing
  ( API
  , api
  , server
  ) where

import qualified AccessCode.Controller as AccessCode
import App (AppT)
import Data.Text (Text)
import qualified Home.Controller as Home
import RouteTypes (WithAuthCookie)
import Servant
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html5 (Html)

type API = MarketingAPI

type MarketingAPI = Home
               :<|> "access-code" :> AccessCode

type Home = Get '[HTML] Html
type AccessCode = QueryParam "code" Text :> Get '[HTML] (WithAuthCookie Html)

server :: ServerT API AppT
server = Home.showA
    :<|> AccessCode.showA

api :: Proxy API
api = Proxy
