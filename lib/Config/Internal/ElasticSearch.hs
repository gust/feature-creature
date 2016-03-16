module Config.Internal.ElasticSearch
( ElasticSearchConfig (..)
, readElasticSearchConfig
) where

import Data.Maybe (fromJust)
import System.Environment (getEnv)

data ElasticSearchConfig =
  ElasticSearchConfig { getESUrl     :: String
                      , getIndexName :: String
                      , getShardCount :: Int
                      , getReplicaCount :: Int
                      } deriving (Show)

readElasticSearchConfig :: IO ElasticSearchConfig
readElasticSearchConfig =
  ElasticSearchConfig
    <$> getEnv "FC_ELASTIC_SEARCH_URL"
    <*> getEnv "FC_ELASTIC_SEARCH_INDEX_NAME"
    <*> (fromJust . read <$> getEnv "FC_ELASTIC_SEARCH_SHARD_COUNT")
    <*> (fromJust . read <$> getEnv "FC_ELASTIC_SEARCH_REPLICA_COUNT")

