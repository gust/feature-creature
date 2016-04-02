{-# LANGUAGE OverloadedStrings #-}

module Products.ProductRepo
( ProductRepo (..)
, Git.FileModification (..)
, Git.ParseResult
, Git.parseStatusDiff
, getStatusDiff
, codeRepositoryDir
, fetchRepo
, findProductRepos
, updateRepo
) where

import           CommonCreatures (WithErr)
import           Config.Config (GitConfig (..))
import           Control.Monad (mzero)
import           Control.Monad.Reader (ask, liftIO)
import qualified Data.Aeson as AE
import           Data.Aeson ((.=), (.!=), (.:), (.:?))
import           Data.Text (Text)
import qualified Database.Esqueleto as E
import           Database.Esqueleto ((^.))
import qualified Database.Persist.Postgresql as DB
import           Database.Types (WithDBPool (..))
import qualified Git.Git as Git
import           Models
import           ModelTypes (RepositoryState (..))
import qualified Products.Product as P
import           System.Directory (doesDirectoryExist, createDirectoryIfMissing)

data ProductRepo =
  ProductRepo { getProductId :: Maybe P.ProductID
              , getProductName :: Text
              , getProductRepoUrl :: Text
              , getProductRepoState :: RepositoryState
              , getProductRepoError :: Maybe Text
              } deriving (Show, Eq)

instance AE.ToJSON ProductRepo where
  toJSON (ProductRepo pId pName pRepoUrl pRepoState pRepoError) =
    AE.object [ "productId"   .= pId
              , "productName" .= pName
              , "repoUrl"     .= pRepoUrl
              , "repoState"   .= pRepoState
              , "repoError"   .= pRepoError
              ]

instance AE.FromJSON ProductRepo where
  parseJSON (AE.Object v) =
    ProductRepo
      <$> v .:? "productId" .!= Nothing
      <*> v .: "productName"
      <*> v .: "repoUrl"
      <*> v .:? "repoState" .!= Unready
      <*> v .:? "repoError" .!= Nothing
  parseJSON _          = mzero

findProductRepos :: WithDBPool [ProductRepo]
findProductRepos = ask >>= \pool ->
  (liftIO (DB.runSqlPool findProductReposQuery pool)) >>= (return . (fmap toProductRepo))
  where
    findProductReposQuery =
      E.select $ E.from $ \(repoStatus `E.InnerJoin` prod) -> do
        E.on $ repoStatus ^. RepositoryStatusProductId E.==. prod ^. ProductId
        return (repoStatus, prod)

toProductRepo :: (E.Entity RepositoryStatus, E.Entity P.Product) -> ProductRepo
toProductRepo (rsEntity, prodEntity) =
  ProductRepo { getProductId        = Just $ P.toProductID $ prodEntity
              , getProductName      = productName . P.toProduct $ prodEntity
              , getProductRepoUrl   = productRepoUrl . P.toProduct $ prodEntity
              , getProductRepoState = repositoryStatusState . toProductRepoStatus $ rsEntity
              , getProductRepoError = repositoryStatusError . toProductRepoStatus $ rsEntity
              }

toProductRepoStatus :: DB.Entity RepositoryStatus -> RepositoryStatus
toProductRepoStatus dbEntity = DB.entityVal dbEntity

-- (ReaderT GitConfig (WithErr a)) could be a useful monad here
updateRepo :: P.Product -> P.ProductID -> GitConfig -> WithErr String
updateRepo prod prodID gitConfig =
  (liftIO $ createRequiredDirectories prodID gitConfig)
    >> updateGitRepo (productRepoUrl prod) prodID gitConfig

-- (ReaderT GitConfig (WithErr a)) could be a useful monad here
updateGitRepo :: Text -> P.ProductID -> GitConfig -> WithErr String
updateGitRepo gitUrl prodID gitConfig = do
  let repositoryPath = codeRepositoryDir prodID gitConfig
  doesRepoExist <- liftIO $ doesDirectoryExist repositoryPath
  case doesRepoExist of
    True  -> Git.pull repositoryPath
    False -> Git.clone repositoryPath gitUrl

fetchRepo :: P.ProductID -> GitConfig -> WithErr String
fetchRepo prodID gitConfig =
  let repositoryPath = codeRepositoryDir prodID gitConfig
  in Git.fetch repositoryPath

getStatusDiff :: P.ProductID -> GitConfig -> WithErr String
getStatusDiff prodID gitConfig =
  let repositoryPath = codeRepositoryDir prodID gitConfig
  in Git.statusDiff repositoryPath

-- maybe the combination of a P.ProductID and GitConfig is a ProductRepository?
productDir :: P.ProductID -> GitConfig -> FilePath
productDir prodID gitConfig =
  (repoBasePath gitConfig) ++ "/products/" ++ (show prodID)

-- maybe the combination of a P.ProductID and GitConfig is a ProductRepository?
codeRepositoryDir :: P.ProductID -> GitConfig -> FilePath
codeRepositoryDir prodID gitConfig =
  productDir prodID gitConfig ++ "/repo"

-- maybe the combination of a P.ProductID and GitConfig is a ProductRepository?
createRequiredDirectories :: P.ProductID -> GitConfig -> IO ()
createRequiredDirectories prodID gitConfig =
  createDirectoryIfMissing True (productDir prodID gitConfig)

