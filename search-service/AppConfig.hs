module AppConfig
( AppConfig (..)
, readConfig
) where

import Config (AWSConfig(..), GitConfig(..), ElasticSearchConfig(..))
import System.Environment (getEnv)

data AppConfig =
  AppConfig { getAWSConfig           :: AWSConfig
            , getGitConfig           :: GitConfig
            , getElasticSearchConfig :: ElasticSearchConfig
            , featureFilePath        :: String
            }

readConfig :: IO AppConfig
readConfig = do
  AppConfig
    <$> awsConfiguration
    <*> gitConfiguration
    <*> elasticSearchConfiguration
    <*> getEnv "FC_DATA_FILES_PATH"

awsConfiguration :: IO AWSConfig
awsConfiguration =
  AWSConfig
    <$> getEnv "FC_AWS_ACCESS_KEY"
    <*> getEnv "FC_AWS_SECRET_KEY"
    <*> getEnv "FC_AWS_SQS_URL"

elasticSearchConfiguration :: IO ElasticSearchConfig
elasticSearchConfiguration =
  ElasticSearchConfig
    <$> getEnv "FC_ELASTIC_SEARCH_URL"
    <*> getEnv "FC_ELASTIC_SEARCH_INDEX_NAME"

gitConfiguration :: IO GitConfig
gitConfiguration =
  GitConfig
  <$> getEnv "FC_DATA_FILES_PATH"
