{-# LANGUAGE OverloadedStrings #-}

module SQS where

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
import           Products.Product (IndexableRepo)
import           System.IO

import qualified Data.ByteString.Char8 as Char8

getSQSMessages :: AWSConfig -> Text -> IO [IndexableRepo]
getSQSMessages awsConfig queueName = do
  env <- awsEnv awsConfig

  let url = awsSQSUrl awsConfig queueName

  runResourceT . runAWST env $ do
    ms  <- send (receiveMessage url & rmWaitTimeSeconds ?~ 20)
    let repos  = mapMaybe (view mBody) (ms ^. rmrsMessages)

    liftIO $ putStrLn $ concat $ map Text.unpack repos
    return $ mapMaybe (decode . TLE.encodeUtf8 . DTL.fromStrict) repos

sendSQSMessage :: AWSConfig -> Text -> IndexableRepo -> IO ()
sendSQSMessage awsConfig queueName msg = do
  env <- awsEnv awsConfig

  let url = awsSQSUrl awsConfig queueName

  runResourceT . runAWST env $ do
    let sqsMessage = (Enc.decodeUtf8 . BSL.toStrict $ Aeson.encode msg)
    void $ send (sendMessage url sqsMessage)

awsKeys :: AWSConfig -> Credentials
awsKeys awsConfig =
  FromKeys
    (AccessKey $ Char8.pack (accessKey awsConfig))
    (SecretKey $ Char8.pack (secretKey awsConfig))

awsEnv :: AWSConfig -> IO Env
awsEnv awsConfig = do
  lgr <- logger
  newEnv NorthVirginia (awsKeys awsConfig) <&> envLogger .~ lgr

awsSQSUrl :: AWSConfig -> Text -> Text
awsSQSUrl awsConfig queueName =
  Text.pack $ (sqsUrl awsConfig) ++ (Text.unpack queueName)

logger :: IO Logger
logger = newLogger Debug stdout
