module Config.AWS
( AWSConfig
, getAWSConfig
) where

import System.Environment (getEnv)

data AWSConfig =
  AWSConfig { accessKey        :: String
            , secretKey        :: String
            , sqsUrl           :: String
            } deriving (Show)

getAWSConfig :: IO AWSConfig
getAWSConfig =
  AWSConfig
    <$> getEnv "FC_AWS_ACCESS_KEY"
    <*> getEnv "FC_AWS_SECRET_KEY"
    <*> getEnv "FC_AWS_SQS_URL"
