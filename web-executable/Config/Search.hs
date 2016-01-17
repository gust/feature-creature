module Config.Search
( ElasticSearchConfig
, getElasticSearchConfig
) where

import Config (ElasticSearchConfig (..))
import System.Environment (getEnv)

getElasticSearchConfig :: IO ElasticSearchConfig
getElasticSearchConfig =
  ElasticSearchConfig
    <$> getEnv "FC_ELASTIC_SEARCH_URL"
    <*> getEnv "FC_ELASTIC_SEARCH_INDEX_NAME"
