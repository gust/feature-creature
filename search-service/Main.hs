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
  processJob job
    >>= (liftIO . runExceptT)
    >>= (\result -> processJobResult result deliveryReceipt)

processJob :: Job CodeRepository -> App (WithErr ())
processJob job = do
  case Job.getPayload job of
    CodeRepository _ ->
      ask >>= (\cfg ->
        let gitConfig = getGitConfig cfg
            esConfig  = getElasticSearchConfig cfg
        in return $ Features.indexFeatures (Job.getPayload job) gitConfig esConfig)
    _ ->
      return $ throwError $ "Unprocessable job type: " ++ (Text.unpack $ Job.getJobType job)

processJobResult :: Either String () -> Text.Text -> App ()
processJobResult (Left errStr) _ = liftIO $ putStrLn errStr
processJobResult (Right _) deliveryReceipt =
  reader getAWSConfig
    >>= (liftIO . (deleteSQSMessage deliveryReceipt))

