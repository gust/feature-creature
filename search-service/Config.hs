module Config where

import System.Environment (getEnv)

data AppConfig =
  AppConfig { awsAccessKey     :: String
            , awsSecretKey     :: String
            , awsSQSUrl        :: String
            , elasticSearchUrl :: String
            , featureFilePath  :: String
            }

readConfig :: IO AppConfig
readConfig =
  AppConfig
    <$> getEnv "FC_AWS_ACCESS_KEY"
    <*> getEnv "FC_AWS_SECRET_KEY"
    <*> getEnv "FC_AWS_SQS_URL"
    <*> getEnv "FC_AWS_ELASTIC_SEARCH_URL"
    <*> getEnv "FC_DATA_FILES_PATH"
