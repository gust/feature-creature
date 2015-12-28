{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Products.Product
( Product(..)
, ProductID
, CodeRepository(..)
, createProduct
, codeRepositoryDir
, findProducts
, indexFeaturesJob
, toProduct
, toProductID
) where

import Async.Job (Job(..))
import Data.Aeson as Aeson
import CommonCreatures (WithErr)
import Config (DBConfig, GitConfig, getPool, repoBasePath)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Database.Persist.Postgresql as DB
import GHC.Int (Int64)
import GHC.Generics (Generic)
import qualified Git
import Models
import System.Directory (doesDirectoryExist, createDirectoryIfMissing)

type ProductID = Int64

-- rewrite this using a WithDBConn monad
createProduct :: DBConfig -> GitConfig -> Product -> WithErr ProductID
createProduct dbConfig gitConfig p = do
  prodID <- (liftIO $ createProduct' dbConfig p)
  updateRepo p prodID (repoBasePath gitConfig) >> (return prodID)

-- rewrite this using a WithDBConn monad
createProduct' :: DBConfig -> Product -> IO ProductID
createProduct' dbConfig p =
  let query = DB.insert p
      pool = getPool dbConfig
  in
    DB.runSqlPool query pool >>= return . DB.fromSqlKey

-- rewrite this using a WithDBConn monad
findProducts :: DBConfig -> IO [DB.Entity Product]
findProducts dbConfig =
  let query = DB.selectList ([] :: [DB.Filter Product]) []
      pool = getPool dbConfig
  in
    DB.runSqlPool query pool

productDir :: ProductID -> FilePath
productDir prodID =
  "/products/" ++ (show prodID)

createRequiredDirectories :: FilePath -> ProductID -> IO ()
createRequiredDirectories basePath prodID =
  createDirectoryIfMissing True (basePath ++ productDir prodID)

toProductID :: DB.Entity Product -> ProductID
toProductID dbEntity =
  DB.fromSqlKey . DB.entityKey $ dbEntity

toProduct :: DB.Entity Product -> Product
toProduct dbEntity =
  DB.entityVal dbEntity


data CodeRepository =
  CodeRepository { repoPath :: T.Text
                 } deriving (Show, Generic)

instance ToJSON CodeRepository
instance FromJSON CodeRepository

indexFeaturesJob :: CodeRepository -> Job CodeRepository
indexFeaturesJob codeRepo =
  Job { jobType = "IndexFeatures"
      , payload = codeRepo
      }

codeRepositoryDir :: ProductID -> FilePath
codeRepositoryDir prodID =
  productDir prodID ++ "/repo"

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
