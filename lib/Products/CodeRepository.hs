{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Products.CodeRepository
( CodeRepository(..)
, codeRepositoryDir
, indexFeaturesJob
, updateRepo
) where

import Async.Job (Job(..))
import CommonCreatures (WithErr)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson as Aeson
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Git
import Products.Product (Product (..), ProductID)
import System.Directory (doesDirectoryExist, createDirectoryIfMissing)

data CodeRepository =
  CodeRepository { repoPath :: T.Text
                 } deriving (Show, Generic)

instance ToJSON CodeRepository
instance FromJSON CodeRepository

updateRepo :: Product -> ProductID -> FilePath -> WithErr String
updateRepo prod prodID basePath = do
  let prodRepoPath = basePath ++ codeRepositoryDir prodID
  (liftIO $ createRequiredDirectories basePath prodID) >> updateGitRepo prodRepoPath (productRepoUrl prod)

updateGitRepo :: FilePath -> T.Text -> WithErr String
updateGitRepo repositoryPath gitUrl = do
  doesRepoExist <- liftIO $ doesDirectoryExist repositoryPath
  case doesRepoExist of
    True  -> Git.pull repositoryPath
    False -> Git.clone repositoryPath gitUrl

productDir :: ProductID -> FilePath
productDir prodID =
  "/products/" ++ (show prodID)

codeRepositoryDir :: ProductID -> FilePath
codeRepositoryDir prodID =
  productDir prodID ++ "/repo"

createRequiredDirectories :: FilePath -> ProductID -> IO ()
createRequiredDirectories basePath prodID =
  createDirectoryIfMissing True (basePath ++ productDir prodID)

indexFeaturesJob :: CodeRepository -> Job CodeRepository
indexFeaturesJob codeRepo =
  Job { jobType = "IndexFeatures"
      , payload = codeRepo
      }

