{-# LANGUAGE OverloadedStrings #-}

module Main where

import           App
import           AppConfig (AppConfig(..), readConfig)
import           Async.Job as Job
import           Control.Concurrent (threadDelay)
import           Control.Monad.Reader
import qualified Data.Text as Text
import qualified Features
import           Products.CodeRepository (CodeRepository(..))
import           SQS (getSQSMessages)

main :: IO ()
main = do
  appConfig <- readConfig
  runReaderT processJobs appConfig

processJobs :: App ()
processJobs = do
  cfg    <- ask
  awsCfg <- reader awsConfig

  liftIO $ forever $ do
    enqueuedJobs <- getSQSMessages awsCfg
    forM_ enqueuedJobs $ \enqueuedJob -> do
      case enqueuedJob of
        Left err ->
          liftIO $ putStrLn err
        Right enqJob -> do
          let job = getJob enqJob
          let deliveryReceipt = getDeliveryReceipt enqJob
          runReaderT (processJob job) cfg
    threadDelay 10000

processJob :: Job CodeRepository -> App ()
processJob job = do
  case Job.getPayload job of
    CodeRepository _ -> do
      liftIO $ Features.indexFeatures (Job.getPayload job)
    _ ->
      liftIO . putStrLn $ "Unprocessable job type: " ++ (Text.unpack $ Job.getJobType job)
