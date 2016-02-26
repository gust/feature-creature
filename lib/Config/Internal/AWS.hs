module Config.Internal.AWS
( AWSConfig (..)
, readAWSConfig
) where

import System.Environment (getEnv)

data AWSConfig =
  AWSConfig { accessKey        :: String
            , secretKey        :: String
            , sqsUrl           :: String
            } deriving (Show)

readAWSConfig :: IO AWSConfig
readAWSConfig =
  AWSConfig
    <$> getEnv "FC_AWS_ACCESS_KEY"
    <*> getEnv "FC_AWS_SECRET_KEY"
    <*> getEnv "FC_AWS_SQS_URL"

