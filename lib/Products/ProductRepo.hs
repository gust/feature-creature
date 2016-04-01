{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Products.ProductRepo
( ProductRepo (..)
, findProductRepos
) where

import           Control.Monad (mzero)
import           Control.Monad.Reader (ask, liftIO)
import qualified Data.Aeson as AE
import           Data.Aeson ((.=), (.!=), (.:), (.:?))
import           Data.Text (Text)
import qualified Database.Esqueleto as E
import           Database.Esqueleto ((^.))
import qualified Database.Persist.Postgresql as DB
import           Database.Types (WithDBPool (..))
import           GHC.Generics (Generic)
import           Models
import           ModelTypes (RepositoryState (..))
import qualified Products.Product as P

data ProductRepo =
  ProductRepo { getProductId :: Maybe P.ProductID
              , getProductName :: Text
              , getProductRepoUrl :: Text
              , getProductRepoState :: RepositoryState
              , getProductRepoError :: Maybe Text
              } deriving (Show, Eq, Generic)

instance AE.ToJSON ProductRepo where
  toJSON (ProductRepo pId pName pRepoUrl pRepoState pRepoError) =
    AE.object [ "productId"   .= pId
              , "productName" .= pName
              , "repoUrl"     .= pRepoUrl
              , "repoState"   .= pRepoState
              , "repoError"   .= pRepoError
              ]

instance AE.FromJSON ProductRepo where
  parseJSON (AE.Object v) = ProductRepo <$>
                        v .:? "productId" .!= Nothing<*>
                        v .: "productName" <*>
                        v .: "repoUrl" <*>
                        v .:? "repoState" .!= Unready <*>
                        v .:? "repoError" .!= Nothing
  parseJSON _          = mzero

findProductRepos :: WithDBPool [ProductRepo]
findProductRepos = ask >>= \pool ->
  (liftIO (DB.runSqlPool findProductReposQuery pool)) >>= (return . (fmap toProductRepo))
  where
    findProductReposQuery =
      E.select $ E.from $ \(repoStatus `E.InnerJoin` prod) -> do
        E.on $ repoStatus ^. RepositoryStatusProductId E.==. prod ^. ProductId
        return (repoStatus, prod)

toProductRepo :: (E.Entity RepositoryStatus, E.Entity Product) -> ProductRepo
toProductRepo (rsEntity, prodEntity) =
  ProductRepo { getProductId        = Just $ P.toProductID $ prodEntity
              , getProductName      = productName . P.toProduct $ prodEntity
              , getProductRepoUrl   = productRepoUrl . P.toProduct $ prodEntity
              , getProductRepoState = repositoryStatusState . toProductRepoStatus $ rsEntity
              , getProductRepoError = repositoryStatusError . toProductRepoStatus $ rsEntity
              }

toProductRepoStatus :: DB.Entity RepositoryStatus -> RepositoryStatus
toProductRepoStatus dbEntity = DB.entityVal dbEntity
