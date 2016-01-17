{-# LANGUAGE OverloadedStrings #-}

module Main where

import           App
import           AppConfig (AppConfig(..), readConfig)
import           Async.Job as Job
import           CommonCreatures (WithErr)
import           Control.Concurrent (threadDelay)
import           Control.Monad.Except (runExceptT, throwError)
import           Control.Monad.Reader
import qualified Data.Text as Text
import qualified Features
import           Products.CodeRepository (CodeRepository(..))
import           SQS (getSQSMessages, deleteSQSMessage)

main :: IO ()
main = do
  appConfig <- readConfig
  runReaderT processJobs appConfig

processJobs :: App ()
processJobs = do
  cfg    <- ask
  awsCfg <- reader getAWSConfig

  liftIO $ forever $ do
    enqueuedJobs <- getSQSMessages awsCfg
    forM_ enqueuedJobs $ \enqueuedJob -> do
      case enqueuedJob of
        Left err ->
          liftIO $ putStrLn err
        Right (EnqueuedJob job deliveryReceipt) -> do
          result <- runReaderT (processJob job) cfg >>= liftIO . runExceptT
          case result of
            Left errStr -> liftIO $ putStrLn errStr
            Right _     -> deleteSQSMessage deliveryReceipt awsCfg >> return ()
    threadDelay 10000

processJob :: Job CodeRepository -> App (WithErr ())
processJob job = do
  case Job.getPayload job of
    CodeRepository _ -> do
      gitConfig <- reader getGitConfig
      esConfig <- reader getElasticSearchConfig
      return $ Features.indexFeatures (Job.getPayload job) gitConfig esConfig
    _ ->
      return $ throwError $ "Unprocessable job type: " ++ (Text.unpack $ Job.getJobType job)
