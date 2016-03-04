{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Products.CodeRepository
( CodeRepository (..)
, Git.FileModification (..)
, Git.ParseResult
, Git.parseStatusDiff
, getStatusDiff
, codeRepositoryDir
, fetchRepo
, indexProductFeaturesJob
, updateRepo
) where

import Async.Job (Job(..))
import CommonCreatures (WithErr)
import Config.Config (GitConfig (..))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson as Aeson
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Git.Git as Git
import Products.Product (Product (..), ProductID)
import System.Directory (doesDirectoryExist, createDirectoryIfMissing)

data CodeRepository =
  CodeRepository { getProductID :: ProductID
                 } deriving (Show, Generic)

instance ToJSON CodeRepository
instance FromJSON CodeRepository

-- (ReaderT GitConfig (WithErr a)) could be a useful monad here
updateRepo :: Product -> ProductID -> GitConfig -> WithErr String
updateRepo prod prodID gitConfig =
  (liftIO $ createRequiredDirectories prodID gitConfig)
    >> updateGitRepo (productRepoUrl prod) prodID gitConfig

-- (ReaderT GitConfig (WithErr a)) could be a useful monad here
updateGitRepo :: T.Text -> ProductID -> GitConfig -> WithErr String
updateGitRepo gitUrl prodID gitConfig = do
  let repositoryPath = codeRepositoryDir prodID gitConfig
  doesRepoExist <- liftIO $ doesDirectoryExist repositoryPath
  case doesRepoExist of
    True  -> Git.pull repositoryPath
    False -> Git.clone repositoryPath gitUrl

fetchRepo :: ProductID -> GitConfig -> WithErr String
fetchRepo prodID gitConfig =
  let repositoryPath = codeRepositoryDir prodID gitConfig
  in
    Git.fetch repositoryPath

getStatusDiff :: ProductID -> GitConfig -> WithErr String
getStatusDiff prodID gitConfig =
  let repositoryPath = codeRepositoryDir prodID gitConfig
  in
    Git.statusDiff repositoryPath

-- maybe the combination of a ProductID and GitConfig is a ProductRepository?
productDir :: ProductID -> GitConfig -> FilePath
productDir prodID gitConfig =
  (repoBasePath gitConfig) ++ "/products/" ++ (show prodID)

-- maybe the combination of a ProductID and GitConfig is a ProductRepository?
codeRepositoryDir :: ProductID -> GitConfig -> FilePath
codeRepositoryDir prodID gitConfig =
  productDir prodID gitConfig ++ "/repo"

-- maybe the combination of a ProductID and GitConfig is a ProductRepository?
createRequiredDirectories :: ProductID -> GitConfig -> IO ()
createRequiredDirectories prodID gitConfig =
  createDirectoryIfMissing True (productDir prodID gitConfig)

indexProductFeaturesJob :: CodeRepository -> Job CodeRepository
indexProductFeaturesJob codeRepo = Job "IndexFeatures" codeRepo

