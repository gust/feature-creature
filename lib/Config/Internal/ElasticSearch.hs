module Config.Internal.ElasticSearch
( ElasticSearchConfig (..)
, readElasticSearchConfig
) where

import System.Environment (getEnv, lookupEnv)

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
    <*> ((maybe (1 :: Int) read) <$> lookupEnv "FC_ELASTIC_SEARCH_SHARD_COUNT")
    <*> ((maybe (0 :: Int) read) <$> lookupEnv "FC_ELASTIC_SEARCH_REPLICA_COUNT")
