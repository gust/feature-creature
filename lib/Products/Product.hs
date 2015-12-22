{-# LANGUAGE DeriveGeneric #-}

module Products.Product
  ( Product(..)
  , ProductID
  , CodeRepository(..)
  , createProduct
  , findProducts
  , codeRepositoryDir
  , toProduct
  , toProductID
  ) where

  import Data.Aeson as Aeson
  import CommonCreatures (WithErr)
  import qualified Config as Cfg
  import Control.Monad.IO.Class (liftIO)
  import qualified Data.Text as T
  import Database (runDB)
  import qualified Database.Persist.Postgresql as DB
  import GHC.Int (Int64)
  import GHC.Generics (Generic)
  import qualified Git
  import Models
  import System.Directory (doesDirectoryExist, createDirectoryIfMissing)

  type ProductID = Int64

  createProduct :: Product -> WithErr ProductID
  createProduct p = do
    prodID <- (liftIO $ createProduct' p)
    updateRepo p prodID >> (return prodID)

  createProduct' :: Product -> IO ProductID
  createProduct' p = do
    newProduct <- runDB $ DB.insert p
    return $ DB.fromSqlKey newProduct

  findProducts :: IO [DB.Entity Product]
  findProducts = do
    allProducts <- runDB $ DB.selectList ([] :: [DB.Filter Product]) []
    return $ allProducts

  productDir :: ProductID -> IO FilePath
  productDir prodID =
    (++ productDirectory) <$> Cfg.gitRepositoryStorePath
    where
      productDirectory = "/products/" ++ (show prodID)

  createRequiredDirectories :: ProductID -> IO ()
  createRequiredDirectories prodID =
    productDir prodID >>= createDirectoryIfMissing True

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

  codeRepositoryDir :: ProductID -> IO FilePath
  codeRepositoryDir prodID =
    (++ "/repo") <$> (productDir prodID)

  updateRepo :: Product -> ProductID -> WithErr String
  updateRepo prod prodID = do
    prodRepoPath <- liftIO $ codeRepositoryDir prodID
    (liftIO $ createRequiredDirectories prodID) >> updateGitRepo prodRepoPath (productRepoUrl prod)

  updateGitRepo :: FilePath -> T.Text -> WithErr String
  updateGitRepo repositoryPath gitUrl = do
    doesRepoExist <- liftIO $ doesDirectoryExist repositoryPath
    case doesRepoExist of
      True  -> Git.pull repositoryPath
      False -> Git.clone repositoryPath gitUrl
