module AppConfig
( AppConfig (..)
) where

data AppConfig =
  AppConfig { awsConfig :: String }

{- readConfig :: IO AppConfig -}
{- readConfig = -}
  {- AppConfig -}
    {- <$> awsConfiguration -}
    {- <*> getEnv "FC_DATA_FILES_PATH" -}

{- awsConfiguration :: IO AWSConfig -}
{- awsConfiguration = -}
  {- AWSConfig -}
    {- <$> getEnv "FC_AWS_ACCESS_KEY" -}
    {- <*> getEnv "FC_AWS_SECRET_KEY" -}
    {- <*> getEnv "FC_AWS_SQS_URL" -}
    {- <*> getEnv "FC_AWS_ELASTIC_SEARCH_URL" -}
