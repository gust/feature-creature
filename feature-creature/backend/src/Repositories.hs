module Repositories
( Repository (..)
, RepositoryForm (..)
, RepositoryOwner (..)
, toRepository
, toRepositoryOwner
) where

import Data.Aeson
  ( ToJSON
  , FromJSON
  , (.=)
  , (.:)
  , (.:?)
  )
import qualified Data.Aeson as AE
import Data.Int (Int64)
import Data.Text (Text)
import qualified GitHub.Endpoints.Repos as GH

data Repository =
  Repository { getId       :: Int64
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
  RepositoryOwner { roGetId        :: Int64
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

data RepositoryForm =
  RepositoryForm { getRepositoryFormId :: Int64
                 , getRepositoryFormName :: Text
                 , getRepositoryFormOwnerName :: Text
                 , getRepositoryFormUrl :: Text
                 , getRepositoryFormHtmlUrl :: Text
                 , getRepositoryFormSSHUrl :: Maybe Text
                 , getRepositoryFormCloneUrl :: Maybe Text
                 }
  deriving (Show, Eq, Ord)

instance ToJSON RepositoryForm where
  toJSON RepositoryForm{..} =
    AE.object [ "id"         .= getRepositoryFormId
              ,  "name"      .= getRepositoryFormName
              ,  "ownerName" .= getRepositoryFormOwnerName
              ,  "url"       .= getRepositoryFormUrl
              ,  "htmlUrl"   .= getRepositoryFormHtmlUrl
              ,  "sshUrl"    .= getRepositoryFormSSHUrl
              ,  "cloneUrl"  .= getRepositoryFormCloneUrl
              ]

instance FromJSON RepositoryForm where
  parseJSON = AE.withObject "repositoryForm" $ \v -> do
    rId        <- v .: "id"
    rName      <- v .: "name"
    rOwnerName <- v .: "ownerName"
    rUrl       <- v .: "url"
    rHtmlUrl   <- v .: "htmlUrl"
    rSSHUrl    <- v .:? "sshUrl"
    rCloneUrl  <- v .:? "cloneUrl"
    return (RepositoryForm rId rName rOwnerName rUrl rHtmlUrl rSSHUrl rCloneUrl)

toRepository :: GH.Repo -> Repository
toRepository repo =
  Repository
    (toID . GH.untagId $ GH.repoId repo)
    (GH.getUrl . GH.repoUrl $ repo)
    (GH.untagName $ GH.repoName repo)
    (GH.getUrl . GH.repoHtmlUrl $ repo)
    (fmap GH.getUrl $ GH.repoSshUrl repo)
    (fmap GH.getUrl $ GH.repoCloneUrl repo)
    (GH.getUrl . GH.repoHooksUrl $ repo)
    (toRepositoryOwner $ GH.repoOwner repo)

toRepositoryOwner :: GH.SimpleOwner -> RepositoryOwner
toRepositoryOwner owner =
  RepositoryOwner
    (toID . GH.untagId $ GH.simpleOwnerId owner)
    (GH.untagName $ GH.simpleOwnerLogin owner)
    (GH.getUrl . GH.simpleOwnerUrl $ owner)
    (GH.getUrl . GH.simpleOwnerAvatarUrl $ owner)

toID :: Int -> Int64
toID = fromIntegral
