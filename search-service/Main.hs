{-# LANGUAGE OverloadedStrings #-}

module Main where

import App
import AppConfig (AppConfig(..), readConfig)
import Config (AWSConfig(..))
import Control.Concurrent (threadDelay)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader
import Features.Feature as F
import Indexer

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.AWS
import           Data.Aeson
import           Data.Maybe
import           Data.Monoid
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Products.Product (IndexableRepo(..))
import           Network.AWS.SQS
import           SQS
import           System.IO


main :: IO ()
main = do
  appConfig <- readConfig
  runReaderT indexFeatures appConfig

indexFeatures :: App ()
indexFeatures = do
  forever $ do
    let say = liftIO . putStrLn

    awsCfg   <- reader awsConfig
    basePath <- reader featureFilePath

    let awsAccessKey = accessKey awsCfg
    let awsSecretKey = secretKey awsCfg
    let awsSQSUrl    = sqsUrl awsCfg

    repoPaths <- liftIO $ getSQSMessages awsCfg "feature-creature"

    say $ "Got repo path from enqueued message: " ++ (show repoPaths)

    {- let featureFileBasePath = basePath ++ repoPath -}
    {- -- this could become useful as log output -}
    {- say $ "Finding feature files at: " ++ featureFileBasePath -}
    {- featureFiles <- liftIO $ runExceptT $ F.findFeatureFiles featureFileBasePath -}
    {- case featureFiles of -}
      {- Left errorStr -> -}
        {- say errorStr -}
      {- Right features -> -}
        {- liftIO $ indexFeatures' $ map (\featurePath -> featureFileBasePath ++ featurePath) features -}
    lift $ liftIO $ threadDelay 10000

getRepoPath' :: String -> String -> String -> IO String
getRepoPath' _ _ _ = do
  liftIO $ putStrLn "Enter features path: " >> getLine

