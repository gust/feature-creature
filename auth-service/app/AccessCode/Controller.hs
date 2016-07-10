module AccessCode.Controller
  ( showA
  ) where

import App (AppT, AppConfig (..))
import Auth0.Access (AccessCode, AccessToken (..), exchangeAccessCode)
import Auth0.User (User (..), getUser)
import Control.Monad.Reader
import Control.Monad.Except (ExceptT, runExceptT)
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import qualified Data.Text.Encoding as TE
import Errors
import Servant
import qualified Users.Api as UsersAPI

showA :: Maybe AccessCode -> AppT ()
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
          let headers = [ ("Location", TE.encodeUtf8 getMarketingSiteUrl)
                        , ("Set-Cookie", authCookie token)
                        ]
          throwError $ err301 { errHeaders = headers }

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
authCookie AccessToken{..} = "token=" <> TE.encodeUtf8 getIdToken

runAppIO :: MonadIO m => ExceptT e IO a -> m (Either e a)
runAppIO f = liftIO $ runExceptT f
