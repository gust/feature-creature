module AppConfig where

import Config (AWSConfig(..), GitConfig(..))
import System.Environment (getEnv)

data AppConfig =
  AppConfig { getAWSConfig     :: AWSConfig
            , getGitConfig     :: GitConfig
            , featureFilePath  :: String
            }

readConfig :: IO AppConfig
readConfig = do
  AppConfig
    <$> awsConfiguration
    <*> gitConfiguration
    <*> getEnv "FC_DATA_FILES_PATH"

awsConfiguration :: IO AWSConfig
awsConfiguration =
  AWSConfig
    <$> getEnv "FC_AWS_ACCESS_KEY"
    <*> getEnv "FC_AWS_SECRET_KEY"
    <*> getEnv "FC_AWS_SQS_URL"

gitConfiguration :: IO GitConfig
gitConfiguration =
  GitConfig
  <$> getEnv "FC_DATA_FILES_PATH"
