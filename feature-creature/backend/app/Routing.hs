{-# LANGUAGE TypeFamilies #-}

module Routing
  ( API
  , featureCreatureApi
  , server
  , genAuthServerContext
  ) where


import App (AppT, AppConfig (..))
import qualified App.Controller as App
import Control.Monad.Except (runExceptT, liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Text (Text)
import qualified Data.Text.Encoding as TLE
import Errors (AppError (..))
import qualified Errors as E
import JWT (JWT)
import qualified JWT
import Network.Wai (Request, requestHeaders)
import qualified Products.Controller as Products
import qualified Repositories.Controller as Repositories
import Servant
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import Servant.Server.Experimental.Auth()
import Users.Api (User (..))
import qualified Users.Api as Users

type API = AuthProtect "auth-token-req" :> WebAppAPI
      :<|> AuthProtect "auth-token-req" :> ProductsAPI
      :<|> AuthProtect "auth-token-req" :> RepositoriesAPI

type WebAppAPI       = App.AppAPI
type ProductsAPI     = "api" :> "products" :> Products.ProductsAPI
type RepositoriesAPI = "api" :> "repositories" :> Repositories.RepositoriesAPI

type instance AuthServerData (AuthProtect "auth-token-req") = User

genAuthServerContext :: AppConfig
                     -> Context '[ AuthHandler Request User ]
genAuthServerContext cfg = (authReqHandler cfg) :. EmptyContext

server :: ServerT API AppT
server = App.showA
    :<|> Products.indexA
    :<|> Repositories.indexA

featureCreatureApi :: Proxy API
featureCreatureApi = Proxy

authReqHandler :: AppConfig -> AuthHandler Request User
authReqHandler AppConfig{..} =
  let handler req = case lookup "Cookie" (requestHeaders req) of
        Nothing     -> E.raiseAppError AuthenticationRequired
        Just cookie -> do
          mUser <- liftIO $ loadCurrentUser getUsersApiConfig cookie
          case mUser of
            Nothing -> E.raiseAppError AuthenticationRequired
            (Just user) -> return user
  in mkAuthHandler handler

loadCurrentUser :: Users.Config -> ByteString -> IO (Maybe User)
loadCurrentUser cfg cookie =
  case findAuthToken cookie >>= parseJWT >>= JWT.getSubject of
    Nothing       -> return Nothing
    (Just authId) -> fetchUser cfg authId

-- TODO: This will not be maintainable
--       What does it look like when we have multiple cookies?
findAuthToken :: ByteString -> Maybe ByteString
findAuthToken allCookies =
  case C.split '=' allCookies of
    ("token":tok:_) -> Just tok
    _               -> Nothing

parseJWT :: ByteString -> Maybe JWT
parseJWT encJWT = eitherToMaybe $ JWT.decodeJWT (TLE.decodeUtf8 encJWT)

fetchUser :: Users.Config -> Text -> IO (Maybe User)
fetchUser cfg authId = eitherToMaybe <$> (runExceptT $ Users.findUser authId cfg)

eitherToMaybe :: Either l r -> Maybe r
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right r) = Just r
