module AccessCode.Controller
  ( showA
  ) where

import qualified AccessCode.View as V
import App (AppT, AppConfig (..))
import Auth0.Access (AccessCode, AccessToken (..), exchangeAccessCode)
import Auth0.User (User (..), getUser)
import Control.Monad.Reader
import Control.Monad.Except (ExceptT, runExceptT)
import Data.ByteString.Lazy (ByteString)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Errors
import Text.Blaze.Html5 (Html)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import RouteTypes (WithAuthCookie)
import Servant
import qualified Users.Api as UsersAPI

showA :: Maybe AccessCode -> AppT (WithAuthCookie Html)
showA Nothing     = raiseAppError $ BadRequest "Missing access code"
showA (Just code) = ask >>= \AppConfig{..} -> do
  eToken <- fetchToken code
  case eToken of
    (Left err)    -> raiseAppError $ BadRequest (pack . show $ err)
    (Right token) -> do
      eUser  <- fetchUser token
      case eUser of
        (Left err)   -> raiseAppError $ BadRequest (pack . show $ err)
        (Right user) -> do
          _ <- findOrCreateNewUser user
          return $ addHeader (authCookie token) (V.renderShowA user)

findOrCreateNewUser :: User -> AppT (Either Text UsersAPI.User)
findOrCreateNewUser auth0User = ask >>= \AppConfig{..} -> do
  eUser <- runAppIO $ UsersAPI.findUser (userID auth0User) getUsersApiConfig
  case eUser of
    (Right u) -> return $ Right u
    (Left _)  -> createNewUser auth0User

createNewUser :: User -> AppT (Either Text UsersAPI.User)
createNewUser auth0User = ask >>= \AppConfig{..} -> do
  runAppIO $ UsersAPI.createUser
    (UsersAPI.UserForm Nothing (userID auth0User) (email auth0User) (name auth0User))
    getUsersApiConfig

fetchToken :: AccessCode -> AppT (Either Text AccessToken)
fetchToken code = ask >>= \AppConfig{..} ->
  runAppIO $ exchangeAccessCode code getAuthConfig

fetchUser :: AccessToken -> AppT (Either Text User)
fetchUser token = ask >>= \AppConfig{..} ->
  runAppIO $ getUser token getAuthConfig

authCookie :: AccessToken -> ByteString
authCookie AccessToken{..} = "token=" <> encode getIdToken
  where
    encode = TLE.encodeUtf8 . TL.fromStrict

runAppIO :: MonadIO m => ExceptT e IO a -> m (Either e a)
runAppIO f = liftIO $ runExceptT f
