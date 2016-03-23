module AppConfig
( AppConfig (..)
, ElasticSearchConfig (..)
, GitConfig (..)
, RabbitMQConfig (..)
, readConfig
) where

import Config.Config
import System.Environment (getEnv)

data AppConfig =
  AppConfig { getGitConfig           :: GitConfig
            , getElasticSearchConfig :: ElasticSearchConfig
            , featureFilePath        :: String
            , getRabbitMQConfig      :: RabbitMQConfig
            }

readConfig :: IO AppConfig
readConfig = do
  AppConfig
    <$> readGitConfig
    <*> readElasticSearchConfig
    <*> getEnv "FC_DATA_FILES_PATH"
    <*> readRabbitMQConfig

