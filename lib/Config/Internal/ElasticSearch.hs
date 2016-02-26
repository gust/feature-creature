module Config.Internal.ElasticSearch
( ElasticSearchConfig (..)
, readElasticSearchConfig
) where

import System.Environment (getEnv)

data ElasticSearchConfig =
  ElasticSearchConfig { getESUrl     :: String
                      , getIndexName :: String
                      } deriving (Show)

readElasticSearchConfig :: IO ElasticSearchConfig
readElasticSearchConfig =
  ElasticSearchConfig
    <$> getEnv "FC_ELASTIC_SEARCH_URL"
    <*> getEnv "FC_ELASTIC_SEARCH_INDEX_NAME"
