{-# LANGUAGE OverloadedStrings #-}

-- A note about how AWS SQS works:
-- A call to getSQSMessages will return a list of jobs.
-- These jobs are all referenced by the same delivery receipt.
-- Deleting the jobs referenced by the delivery receipt deletes ALL jobs referenced, duh.
-- Not sure how to handle a case where some of the jobs in the list fail to process
-- where others succeed.

module SQS
( getSQSMessages
, sendSQSMessage
, deleteSQSMessage
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

-- (ReaderT AWSConfig (IO a)) could be a useful monad here (WithAWSConn)
getSQSMessages :: FromJSON a => AWSConfig -> IO [Either String (EnqueuedJob a)]
getSQSMessages awsConfig = do
  let url = awsSQSUrl awsConfig
  env <- awsEnv awsConfig
  runResourceT . runAWST env $ do
    ms  <- send (receiveMessage url & rmWaitTimeSeconds ?~ 20)
    return $ map parseMessage (ms ^. rmrsMessages)

-- (ReaderT AWSConfig (IO a)) could be a useful monad here (WithAWSConn)
-- ignores failure
sendSQSMessage :: ToJSON a => Job a -> AWSConfig -> IO ()
sendSQSMessage job awsConfig = do
  let url = awsSQSUrl awsConfig
  env <- awsEnv awsConfig
  runResourceT . runAWST env $ do
    let msg = sendMessage url (encodeJob job)
    void $ send msg

-- (ReaderT AWSConfig (IO a)) could be a useful monad here (WithAWSConn)
-- ignores failure
deleteSQSMessage :: Text -> AWSConfig -> IO ()
deleteSQSMessage deliveryReceipt awsConfig = do
  let url = awsSQSUrl awsConfig
  env <- awsEnv awsConfig
  runResourceT . runAWST env $ do
    let msg = deleteMessage url deliveryReceipt
    void $ send msg

parseMessage :: FromJSON a => Message -> Either String (EnqueuedJob a)
parseMessage msg =
  EnqueuedJob
  <$> parseMessageBody (view mBody msg)
  <*> parseDeliveryReceipt (view mReceiptHandle msg)

parseMessageBody :: FromJSON a => Maybe Text -> Either String (Job a)
parseMessageBody Nothing        = Left "Unable to parse message body"
parseMessageBody (Just jobJson) = decodeJob jobJson

parseDeliveryReceipt :: Maybe Text -> Either String (Text)
parseDeliveryReceipt Nothing                = Left "Unable to parse message receipt"
parseDeliveryReceipt (Just deliveryReceipt) = Right deliveryReceipt

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
