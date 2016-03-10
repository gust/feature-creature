{-# LANGUAGE OverloadedStrings #-}

module Main where

import App
import AppConfig (AppConfig(..), readConfig)
import Async.Job as Job
import CommonCreatures (WithErr)
import Control.Concurrent (threadDelay)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Reader
import qualified Data.Text as Text
import Features.Feature (FeatureFile, findFeatureFiles)
import qualified Indexer
import Products.CodeRepository (CodeRepository(..), codeRepositoryDir)
import Products.Product (ProductID)
import SQS (getSQSMessages, deleteSQSMessage)

main :: IO ()
main = readConfig >>= runReaderT processJobs

processJobs :: App ()
processJobs =
  forever $ do
    reader getAWSConfig
      >>= (liftIO . getSQSMessages)
      >>= (mapM_ processEnqueuedJob)
      >>  (liftIO $ threadDelay 10000)

processEnqueuedJob :: Either String (EnqueuedJob CodeRepository) -> App ()
processEnqueuedJob (Left err) = liftIO $ putStrLn err
processEnqueuedJob (Right (EnqueuedJob job deliveryReceipt)) = do
  processJob (Job.getPayload job) job
    >>= (liftIO . runExceptT)
    >>= (\result -> processJobResult result deliveryReceipt)

processJob :: CodeRepository -> Job a -> App (WithErr ())
processJob (CodeRepository productID) _ =
  (featureFiles productID)
    >>= (liftIO . runExceptT)
    >>= (\files -> indexFeatures files productID)
processJob _ job =
  return
    $ throwError
    $ "Unprocessable job type: " ++ (Text.unpack $ Job.getJobType job)

processJobResult :: Either String () -> Text.Text -> App ()
processJobResult (Left errStr) _ = liftIO $ putStrLn errStr
processJobResult (Right _) deliveryReceipt =
  reader getAWSConfig
    >>= (liftIO . (deleteSQSMessage deliveryReceipt))

featureFiles :: ProductID -> App (WithErr [FeatureFile])
featureFiles productID =
  (reader getGitConfig)
    >>= (\gitConfig -> return $ findFeatureFiles (codeRepositoryDir productID gitConfig))

indexFeatures :: Either String [FeatureFile] -> ProductID -> App (WithErr ())
indexFeatures (Left err) _ = return $ throwError err
indexFeatures (Right features) productID =
  ask
    >>= (\cfg -> (liftIO $ Indexer.indexFeatures features productID (getGitConfig cfg) (getElasticSearchConfig cfg)))
    >> return (return ())

