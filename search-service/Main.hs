{-# LANGUAGE OverloadedStrings #-}

module Main where

import           App
import           AppConfig (AppConfig(..), readConfig)
import           Async.Job
import           Control.Concurrent (threadDelay)
import           Control.Monad.Except (runExceptT)
import           Control.Monad.Reader
import qualified Data.Text as Text
import           Features.Feature as F
import qualified Indexer
import           Products.Product (CodeRepository(..))
import           SQS

main :: IO ()
main = do
  appConfig <- readConfig
  runReaderT processJobs appConfig

processJobs :: App ()
processJobs = do
  awsCfg   <- reader awsConfig
  basePath <- reader featureFilePath

  liftIO $ forever $ do
    let say = putStrLn

    jobs <- getSQSMessages awsCfg
    say $ "Got enqueued jobs: " ++ (show jobs)

    forM_ jobs $ \job -> do
      case job of
        Left err ->
          putStrLn err
        Right j ->
          case payload j of
            CodeRepository repositoryPath -> do
              let featureFileBasePath = basePath ++ (Text.unpack repositoryPath)
              -- this could become useful as log output
              putStrLn $ "Finding feature files at: " ++ featureFileBasePath

              featureFiles <- runExceptT $ F.findFeatureFiles featureFileBasePath
              case featureFiles of
                Left errorStr ->
                  putStrLn errorStr
                Right features ->
                  Indexer.indexFeatures $ map (\featurePath -> featureFileBasePath ++ featurePath) features
            _ ->
              say $ "Unprocessable job type: " ++ (Text.unpack $ jobType j)

    threadDelay 10000
