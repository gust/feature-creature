{-# LANGUAGE OverloadedStrings #-}

module Main where

import           App
import           AppConfig (AppConfig(..), readConfig)
import           Async.Job as Job
import           Control.Concurrent (threadDelay)
import           Control.Monad.Except (runExceptT)
import           Control.Monad.Reader
import qualified Data.Text as Text
import           Features.Feature as F
import qualified Indexer
import           Products.CodeRepository (CodeRepository(..))
import           SQS

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
        Right job ->
          runReaderT (processJob job) cfg
    threadDelay 10000

processJob :: Job CodeRepository -> App ()
processJob job = do
  let say = liftIO . putStrLn
  case Job.payload job of
    CodeRepository repositoryPath -> do
      let featureFileBasePath = Text.unpack repositoryPath
      say $ "Finding feature files at: " ++ featureFileBasePath

      featureFiles <- liftIO $ runExceptT $ F.findFeatureFiles featureFileBasePath
      case featureFiles of
        Left errorStr ->
          liftIO $ putStrLn errorStr
        Right features ->
          liftIO $ Indexer.indexFeatures $ map (\featurePath -> featureFileBasePath ++ featurePath) features
    _ ->
      say $ "Unprocessable job type: " ++ (Text.unpack $ jobType job)
