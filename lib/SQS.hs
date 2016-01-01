{-# LANGUAGE OverloadedStrings #-}

module SQS
( getSQSMessages
, sendSQSMessage
) where

import           Async.Job as Job
import           Config
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.AWS
import           Data.Aeson as Aeson
import           Data.Maybe
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Network.AWS.SQS
import           System.IO

import qualified Data.ByteString.Char8 as Char8

-- (ReaderT AWSConfig (IO a)) could be a useful monad here (WithAWSConn)
getSQSMessages :: FromJSON a => AWSConfig -> IO [Either String (Job a)]
getSQSMessages awsConfig = do
  let url = awsSQSUrl awsConfig
  env <- awsEnv awsConfig
  runResourceT . runAWST env $ do
    ms  <- send (receiveMessage url & rmWaitTimeSeconds ?~ 20)
    let jobBodies  = mapMaybe (view mBody) (ms ^. rmrsMessages)
    liftIO $ putStrLn $ "Jobs" ++ (concat $ map Text.unpack jobBodies)
    return $ map decodeJob jobBodies

-- (ReaderT AWSConfig (IO a)) could be a useful monad here (WithAWSConn)
sendSQSMessage :: ToJSON a => Job a -> AWSConfig -> IO ()
sendSQSMessage job awsConfig = do
  let url = awsSQSUrl awsConfig
  env <- awsEnv awsConfig
  runResourceT . runAWST env $ do
    let msg = sendMessage url (encodeJob job)
    void $ send msg

awsKeys :: AWSConfig -> Credentials
awsKeys awsConfig =
  FromKeys
    (AccessKey $ Char8.pack (accessKey awsConfig))
    (SecretKey $ Char8.pack (secretKey awsConfig))

awsEnv :: AWSConfig -> IO Env
awsEnv awsConfig = do
  lgr <- logger
  newEnv NorthVirginia (awsKeys awsConfig) <&> envLogger .~ lgr

awsSQSUrl :: AWSConfig -> Text
awsSQSUrl awsConfig =
  Text.pack (sqsUrl awsConfig)

logger :: IO Logger
logger = newLogger Debug stdout
