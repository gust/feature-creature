module Repositories.Controller
  ( RepositoriesAPI
  , indexA
  ) where

import App (AppT)
import Cookies (parseCookies)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (liftM)
import Data.Aeson
  ( ToJSON
  , FromJSON
  , (.=)
  , (.:)
  )
import qualified Data.Aeson as AE
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Vector (Vector)
import qualified Data.Vector as V
import Errors (AppError (..), raiseAppError)
import Servant
import Users.Api

import qualified GitHub.Auth as GH
import qualified GitHub.Endpoints.Repos as GH

type RepositoriesAPI = Header "Cookie" Text :> Get '[JSON] [Repository]

data Repository =
  Repository { getId       :: Int
             , getUrl      :: Text
             , getName     :: Text
             , getHtmlUrl  :: Text
             , getSSHUrl   :: Maybe Text
             , getCloneUrl :: Maybe Text
             , getHooksUrl :: Text
             , getOwner    :: RepositoryOwner
             }
  deriving (Show, Eq, Ord)

instance ToJSON Repository where
  toJSON Repository{..} =
    AE.object [ "id"       .= getId
              , "name"     .= getName
              , "url"      .= getUrl
              , "htmlUrl"  .= getHtmlUrl
              , "sshUrl"   .= getSSHUrl
              , "cloneUrl" .= getCloneUrl
              , "hooksUrl" .= getHooksUrl
              , "owner"    .= getOwner
              ]

instance FromJSON Repository where
  parseJSON = AE.withObject "repository" $ \v -> do
    rId       <- v .:  "id"
    rName     <- v .: "name"
    rUrl      <- v .: "url"
    rHtmlUrl  <- v .: "htmlUrl"
    rSSHUrl   <- v .: "sshUrl"
    rCloneUrl <- v .: "cloneUrl"
    rHooksUrl <- v .: "hooksUrl"
    rOwner    <- v .: "owner"
    return (Repository rId rUrl rName rHtmlUrl rSSHUrl rCloneUrl rHooksUrl rOwner)

data RepositoryOwner =
  RepositoryOwner { roGetId        :: Int
                  , roGetName      :: Text
                  , roGetUrl       :: Text
                  , roGetAvatarUrl :: Text
                  }
  deriving (Show, Eq, Ord)

instance ToJSON RepositoryOwner where
  toJSON RepositoryOwner{..} =
    AE.object [ "id"        .= roGetId
              , "name"      .= roGetName
              , "url"       .= roGetUrl
              , "avatarUrl" .= roGetAvatarUrl
              ]

instance FromJSON RepositoryOwner where
  parseJSON = AE.withObject "repository" $ \v -> do
    rId        <- v .:  "id"
    rName      <- v .: "name"
    rUrl       <- v .: "url"
    rAvatarUrl <- v .: "avatarUrl"
    return (RepositoryOwner rId rUrl rName rAvatarUrl)

toRepository :: GH.Repo -> Repository
toRepository repo =
  Repository
    (GH.untagId $ GH.repoId repo)
    (GH.repoUrl repo)
    (GH.untagName $ GH.repoName repo)
    (GH.repoHtmlUrl repo)
    (GH.repoSshUrl repo)
    (GH.repoCloneUrl repo)
    (GH.repoHooksUrl repo)
    (toRepositoryOwner $ GH.repoOwner repo)

toRepositoryOwner :: GH.SimpleOwner -> RepositoryOwner
toRepositoryOwner owner =
  RepositoryOwner
    (GH.untagId $ GH.simpleOwnerId owner)
    (GH.untagName $ GH.simpleOwnerLogin owner)
    (GH.simpleOwnerUrl owner)
    (GH.simpleOwnerAvatarUrl owner)

fetchUserRepos :: Text -> IO (Either AppError [Repository])
fetchUserRepos token = liftM transformResult fetchRepos
  where
    transformResult = either (Left . toAppError) (Right . toAppRepo)
    fetchRepos = GH.currentUserRepos (GH.OAuth $ TE.encodeUtf8 token) GH.RepoPublicityAll

toAppRepo :: Vector GH.Repo -> [Repository]
toAppRepo = V.toList . (V.map toRepository)

toAppError :: GH.Error -> AppError
toAppError ghError = ServerError $ Just (T.pack . show $ ghError)


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
