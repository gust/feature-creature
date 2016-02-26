module AppConfig
( AppConfig (..)
, AWSConfig (..)
, ElasticSearchConfig (..)
, GitConfig (..)
, readConfig
) where

import Config.Config
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
    <$> readAWSConfig
    <*> readGitConfig
    <*> readElasticSearchConfig
    <*> getEnv "FC_DATA_FILES_PATH"

