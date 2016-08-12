module Repositories.Query
( fetchUserRepos
) where

import Control.Monad (liftM)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Vector (Vector)
import qualified Data.Vector as V
import Errors (AppError (..))
import Repositories (Repository, toRepository)

import qualified GitHub.Auth as GH
import qualified GitHub.Endpoints.Repos as GH

fetchUserRepos :: Text -> IO (Either AppError [Repository])
fetchUserRepos token = liftM transformResult fetchRepos
  where
    transformResult = either (Left . toAppError) (Right . toAppRepo)
    fetchRepos = GH.currentUserRepos (GH.OAuth $ TE.encodeUtf8 token) GH.RepoPublicityAll

toAppRepo :: Vector GH.Repo -> [Repository]
toAppRepo = V.toList . (V.map toRepository)

toAppError :: GH.Error -> AppError
toAppError ghError = ServerError $ Just (T.pack . show $ ghError)
