{-# LANGUAGE OverloadedStrings #-}

module SQS where

import           Async.Job as Job
import           Config
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.AWS
import           Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import           Data.Maybe
import           Data.Text.Encoding as Enc
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy          as DTL
import           Network.AWS.SQS
import           Products.Product (CodeRepository)
import           System.IO

import qualified Data.ByteString.Char8 as Char8

getSQSMessages :: AWSConfig -> IO [CodeRepository]
getSQSMessages awsConfig = do
  env <- awsEnv awsConfig

  let url = awsSQSUrl awsConfig

  runResourceT . runAWST env $ do
    ms  <- send (receiveMessage url & rmWaitTimeSeconds ?~ 20)
    let repoBodies  = mapMaybe (view mBody) (ms ^. rmrsMessages)

    liftIO $ putStrLn $ "Repo Bodies: " ++ (concat $ map Text.unpack repoBodies)
    return $ mapMaybe (decode . TLE.encodeUtf8 . DTL.fromStrict) repoBodies

sendSQSMessage :: AWSConfig -> CodeRepository -> IO ()
sendSQSMessage awsConfig msg = do
  env <- awsEnv awsConfig

  let url = awsSQSUrl awsConfig

  runResourceT . runAWST env $ do
    let sqsMessage = (Enc.decodeUtf8 . BSL.toStrict $ Aeson.encode msg)
    void $ send (sendMessage url sqsMessage)


getSQSMessages' :: FromJSON a => AWSConfig -> IO [Either String (Job a)]
getSQSMessages' awsConfig = do
  let url = awsSQSUrl awsConfig
  env <- awsEnv awsConfig
  runResourceT . runAWST env $ do
    ms  <- send (receiveMessage url & rmWaitTimeSeconds ?~ 20)
    let jobBodies  = mapMaybe (view mBody) (ms ^. rmrsMessages)
    liftIO $ putStrLn $ "Jobs" ++ (concat $ map Text.unpack jobBodies)
    return $ map decodeJob jobBodies

sendSQSMessage' :: ToJSON a => AWSConfig -> Job a -> IO ()
sendSQSMessage' awsConfig job = do
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
