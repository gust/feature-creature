module Main where

import App
import Config (AppConfig(..), readConfig)
import Control.Concurrent (threadDelay)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader
import Features.Feature as F
import Indexer

main :: IO ()
main = do
  appConfig <- readConfig
  runReaderT indexFeatures appConfig

indexFeatures :: App ()
indexFeatures = do
  forever $ do
    basePath <- reader featureFilePath
    repoPath <- liftIO promptForFeaturePath

    let featureFileBasePath = basePath ++ repoPath
    -- this could become useful as log output
    liftIO $ putStrLn $ "Finding feature files at: " ++ featureFileBasePath
    featureFiles <- liftIO $ runExceptT $ F.findFeatureFiles featureFileBasePath
    case featureFiles of
      Left errorStr ->
        liftIO $ putStrLn errorStr
      Right features ->
        liftIO $ indexFeatures' $ map (\featurePath -> featureFileBasePath ++ featurePath) features
    lift $ liftIO $ threadDelay 1000

promptForFeaturePath :: IO String
promptForFeaturePath = do
  liftIO $ putStrLn "Enter features path: " >> getLine
