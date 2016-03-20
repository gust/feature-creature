module AppConfig
( AppConfig (..)
, AWSConfig (..)
, ElasticSearchConfig (..)
, GitConfig (..)
, RabbitMQConfig (..)
, readConfig
) where

import Config.Config
import System.Environment (getEnv)

data AppConfig =
  AppConfig { getAWSConfig           :: AWSConfig
            , getGitConfig           :: GitConfig
            , getElasticSearchConfig :: ElasticSearchConfig
            , featureFilePath        :: String
            , getRabbitMQConfig      :: RabbitMQConfig
            }

readConfig :: IO AppConfig
readConfig = do
  AppConfig
    <$> readAWSConfig
    <*> readGitConfig
    <*> readElasticSearchConfig
    <*> getEnv "FC_DATA_FILES_PATH"
    <*> readRabbitMQConfig

