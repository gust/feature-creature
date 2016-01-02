{-# LANGUAGE OverloadedStrings #-}

module SQS
( getSQSMessages
, sendSQSMessage
) where

import           Async.Job (Job (..), EnqueuedJob (..), encodeJob, decodeJob)
import           Config
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.AWS
import           Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as Char8
import           Data.Text (Text)
import qualified Data.Text as Text
import           Network.AWS.SQS
import           System.IO

getSQSMessages :: FromJSON a => AWSConfig -> IO [Either String (EnqueuedJob a)]
getSQSMessages awsConfig = do
  let url = awsSQSUrl awsConfig
  env <- awsEnv awsConfig
  runResourceT . runAWST env $ do
    ms  <- send (receiveMessage url & rmWaitTimeSeconds ?~ 20)
    return $ map parseMessage (ms ^. rmrsMessages)

parseMessage :: FromJSON a => Message -> Either String (EnqueuedJob a)
parseMessage msg =
  case view mBody msg of
    Nothing ->
      Left "Unable to parse message body"
    Just jobJson ->
      case decodeJob jobJson of
        Left s ->
          Left s
        Right job ->
          case view mReceiptHandle msg of
            Nothing ->
              Left "Unable to parse message receipt"
            Just receipt ->
              Right $ EnqueuedJob job receipt

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
