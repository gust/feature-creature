module AppConfig where

import Config (AWSConfig(..))
import System.Environment (getEnv)

data AppConfig =
  AppConfig { awsConfig     :: AWSConfig
            , featureFilePath  :: String
            }

readConfig :: IO AppConfig
readConfig =
  AppConfig
    <$> awsConfiguration
    <*> getEnv "FC_DATA_FILES_PATH"

awsConfiguration :: IO AWSConfig
awsConfiguration =
  AWSConfig
    <$> getEnv "FC_AWS_ACCESS_KEY"
    <*> getEnv "FC_AWS_SECRET_KEY"
    <*> getEnv "FC_AWS_SQS_URL"
