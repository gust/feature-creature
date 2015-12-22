{-# LANGUAGE OverloadedStrings #-}

module Main where

import           App
import           AppConfig (AppConfig(..), readConfig)
import           Control.Concurrent (threadDelay)
import           Control.Monad.Except (runExceptT)
import           Control.Monad.Reader
import qualified Data.Text as Text
import           Features.Feature as F
import qualified Indexer
import           Products.Product (repoPath)
import           SQS


main :: IO ()
main = do
  appConfig <- readConfig
  runReaderT indexFeatures appConfig

indexFeatures :: App ()
indexFeatures = do
  forever $ do
    let say = liftIO . putStrLn

    awsCfg         <- reader awsConfig
    basePath       <- reader featureFilePath
    indexableRepos <- liftIO $ getSQSMessages awsCfg "feature-creature"

    say $ "Got repo path from enqueued message: " ++ (show indexableRepos)

    forM_ indexableRepos $ \indexableRepo -> do
      let featureFileBasePath = basePath ++ (Text.unpack $ repoPath indexableRepo)
      -- this could become useful as log output
      say $ "Finding feature files at: " ++ featureFileBasePath

      featureFiles <- liftIO $ runExceptT $ F.findFeatureFiles featureFileBasePath
      case featureFiles of
        Left errorStr ->
          say errorStr
        Right features ->
          liftIO $ Indexer.indexFeatures $ map (\featurePath -> featureFileBasePath ++ featurePath) features

    lift $ liftIO $ threadDelay 10000

