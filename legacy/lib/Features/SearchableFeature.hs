{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Features.SearchableFeature
( SearchableFeature (..)
, createFeaturesIndex
, deleteFeaturesIndex
, indexFeatures
, deleteFeatures
, refreshFeaturesIndex
, searchFeatures
) where

import CommonCreatures
import Config.Config (ElasticSearchConfig(..))
import Control.Exception (Exception, try)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Database.Bloodhound as BH
import Data.Aeson
import Data.Maybe (mapMaybe)
import Data.Text (Text, pack)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Network.HTTP.Client
import Products.Product (ProductID)

data SearchableFeature =
  SearchableFeature { getFeaturePath :: Text
                    , getFeatureText :: Text
                    , getProductID   :: ProductID
                    } deriving (Show, Eq, Generic)

instance ToJSON   SearchableFeature
instance FromJSON SearchableFeature

createFeaturesIndex :: ElasticSearchConfig -> IO ()
createFeaturesIndex esConfig =
  withBH' esConfig (createIndex (indexSettings esConfig) (indexName esConfig))
    >>= putStrLn . ("ElasticSearch IndexCreated " ++) . show

deleteFeaturesIndex :: ElasticSearchConfig -> IO ()
deleteFeaturesIndex esConfig =
  withBH' esConfig (deleteIndex (indexName esConfig))
    >>= putStrLn . ("ElasticSearch IndexDeleted " ++) . show

refreshFeaturesIndex :: ElasticSearchConfig -> IO ()
refreshFeaturesIndex esConfig =
  withBH' esConfig (refreshIndex (indexName esConfig))
    >>= putStrLn . ("ElasticSearch RefreshIndex " ++) . show

indexFeatures :: [SearchableFeature] -> ElasticSearchConfig -> WithErr ()
indexFeatures searchableFeatures esConfig =
  let ops = map (createBulkIndex (getIndexName esConfig)) searchableFeatures
  in liftIO $ withBH'' esConfig (bulk (createStream ops)) >>= \result ->
    case result of
      (Left err) -> throwError err
      (Right r)  -> (liftIO $ putStrLn . show $ r) >> return ()

-- TODO: better identify [Text] as the ID of the document
deleteFeatures :: [Text] -> ElasticSearchConfig -> WithErr ()
deleteFeatures docIDs esConfig =
  let ops = map (createBulkDelete (getIndexName esConfig)) docIDs
  in liftIO $ withBH'' esConfig (bulk (createStream ops)) >>= \result ->
    case result of
      (Left err) -> throwError err
      (Right r)  -> (liftIO $ putStrLn . show $ r) >> return ()

searchFeatures :: ProductID -> Text -> ElasticSearchConfig -> WithErr [SearchableFeature]
searchFeatures prodID queryStr esConfig =
  let index      = IndexName (pack . getIndexName $ esConfig)
      searchTerm = featureTextSearch queryStr prodID
  in (liftIO $ withBH' esConfig $ searchByIndex index searchTerm)
       >>= \reply -> parseSearchResults reply

parseSearchResults :: Reply -> WithErr [SearchableFeature]
parseSearchResults reply =
  let results = eitherDecode (responseBody reply)
  in case fmap (hits . searchHits) results of
    Left str -> throwError str
    Right searchResultHits -> return $ mapMaybe hitSource searchResultHits

featureTextSearch :: Text -> ProductID -> Search
featureTextSearch queryStr prodID =
  mkSearch (Just (featureTextQuery queryStr)) (Just (searchFilter prodID))

featureTextQuery :: Text -> Query
featureTextQuery queryStr =
  QueryMatchQuery $ mkMatchQuery (FieldName "getFeatureText") (QueryString queryStr)

searchFilter :: ProductID -> Filter
searchFilter prodID =
  let searchTerm = (Term "getProductID" (pack $ show prodID))
  in IdentityFilter <&&> BoolFilter (MustMatch searchTerm False)

createBulkIndex :: String -> SearchableFeature -> BulkOperation
createBulkIndex idxName f =
  BulkIndex
    (IndexName (pack idxName))
    (MappingName "feature")
    (DocId (getFeaturePath f))
    (toJSON f)

createBulkDelete :: String -> Text -> BulkOperation
createBulkDelete idxName f =
  BulkDelete
    (IndexName (pack idxName))
    (MappingName "feature")
    (DocId f)

createStream :: [BulkOperation] -> V.Vector BulkOperation
createStream ops = V.fromList ops :: V.Vector BulkOperation

indexName :: ElasticSearchConfig -> IndexName
indexName esConfig = IndexName $ pack (getIndexName esConfig)

indexSettings :: ElasticSearchConfig -> IndexSettings
indexSettings esConfig = IndexSettings (ShardCount (getShardCount esConfig)) (ReplicaCount (getReplicaCount esConfig))

withBH' :: ElasticSearchConfig -> BH IO a -> IO a
withBH' esConfig a =
  withBH defaultManagerSettings (Server (pack (getESUrl esConfig))) a

withBH'' :: Exception e => ElasticSearchConfig -> BH IO a -> IO (Either e a)
withBH'' esConfig a =
  try (withBH defaultManagerSettings (Server (pack (getESUrl esConfig))) a)
