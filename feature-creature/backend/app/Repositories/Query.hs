module Repositories.Query
( fetchUserRepos
, createDeployKey
) where

import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Error.Class (MonadError)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as TE
import Data.Vector (Vector)
import qualified Data.Vector as V
import Errors (AppError (..), raiseAppError)
import Repositories (Repository, toRepository, getName, getOwner, roGetName)
import Servant (ServantErr)
import System.Directory (getHomeDirectory)

import qualified GitHub as GH
import qualified GitHub.Data.Name as GH
import qualified GitHub.Endpoints.Repos as GH
import qualified GitHub.Endpoints.Repos.DeployKeys as GH

fetchUserRepos :: (Monad m, MonadIO m, MonadError ServantErr m) => Text -> m [Repository]
fetchUserRepos token = (liftIO $ fetchUserRepos' token) >>= \eRepos ->
  case eRepos of
    Left err -> raiseAppError err
    Right repos -> return repos

fetchUserRepos' :: Text -> IO (Either AppError [Repository])
fetchUserRepos' token = liftM transformResult fetchRepos
  where
    transformResult = either (Left . toAppError) (Right . toAppRepos)
    fetchRepos = GH.currentUserRepos (GH.OAuth $ TE.encodeUtf8 token) GH.RepoPublicityAll

createDeployKey :: (Monad m, MonadIO m, MonadError ServantErr m) => Text -> Repository -> m GH.RepoDeployKey
createDeployKey token repo = (liftIO $ createDeployKey' token repo) >>= \eDeployKey ->
  case eDeployKey of
    Left err -> raiseAppError err
    Right key -> return key

createDeployKey' :: Text -> Repository -> IO (Either AppError GH.RepoDeployKey)
createDeployKey' token repo = liftM transformResult createKey
  where
    transformResult = either (Left . toAppError) Right
    createKey = deployKey >>= \newDeployKey ->
      GH.createRepoDeployKey'
        (GH.OAuth $ TE.encodeUtf8 token)
        (GH.N (roGetName . getOwner $ repo))
        (GH.N (getName repo))
        newDeployKey

deployKey :: IO GH.NewRepoDeployKey
deployKey =
  let title = "feature-creature deploy key"
      readOnly = True
  in publicKey >>= T.readFile >>= \key ->
      return $ GH.NewRepoDeployKey key title readOnly

publicKey :: IO String
publicKey = liftM (<> "/id_rsa_feature_creature.pub") getHomeDirectory

toAppRepos :: Vector GH.Repo -> [Repository]
toAppRepos = V.toList . (V.map toRepository)

toAppError :: GH.Error -> AppError
toAppError ghError = ServerError $ Just (T.pack . show $ ghError)
