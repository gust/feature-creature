module Repositories.Controller
( RepositoriesAPI
, indexA
) where

import App (AppT)
import Cookies (parseCookies)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Errors (AppError (..), raiseAppError)
import Repositories (Repository)
import Repositories.Query (fetchUserRepos)
import Servant
import Users.Api

type RepositoriesAPI = Header "Cookie" Text :> Get '[JSON] [Repository]

indexA :: User -> Maybe Text -> AppT [Repository]
indexA _ Nothing        = raiseMissingAccessTokenError
indexA _ (Just cookies) = do
  case Map.lookup "access-token" (parseCookies $ TE.encodeUtf8 cookies) of
    Nothing    -> raiseMissingAccessTokenError
    Just token -> (liftIO $ fetchUserRepos token) >>= \eRepos ->
      case eRepos of
        Left err    -> raiseAppError err
        Right repos -> return repos

raiseMissingAccessTokenError :: AppT a
raiseMissingAccessTokenError =
  raiseAppError (BadRequest "Missing access-token cookie")
