module Config.AWS
( AWSConfig
, getAWSConfig
) where

import Config (AWSConfig (..))
import System.Environment (getEnv)

getAWSConfig :: IO AWSConfig
getAWSConfig =
  AWSConfig
    <$> getEnv "FC_AWS_ACCESS_KEY"
    <*> getEnv "FC_AWS_SECRET_KEY"
    <*> getEnv "FC_AWS_SQS_URL"
