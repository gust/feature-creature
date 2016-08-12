module Repositories.Controller
  ( RepositoriesAPI
  , indexA
  ) where

import App (AppT)
import Cookies (parseCookies)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (liftM)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Vector (Vector)
import qualified Data.Vector as V
import Errors (AppError (..), raiseAppError)
import Repositories (Repository, toRepository)
import Servant
import Users.Api

import qualified GitHub.Auth as GH
import qualified GitHub.Endpoints.Repos as GH

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

fetchUserRepos :: Text -> IO (Either AppError [Repository])
fetchUserRepos token = liftM transformResult fetchRepos
  where
    transformResult = either (Left . toAppError) (Right . toAppRepo)
    fetchRepos = GH.currentUserRepos (GH.OAuth $ TE.encodeUtf8 token) GH.RepoPublicityAll

toAppRepo :: Vector GH.Repo -> [Repository]
toAppRepo = V.toList . (V.map toRepository)

toAppError :: GH.Error -> AppError
toAppError ghError = ServerError $ Just (T.pack . show $ ghError)
