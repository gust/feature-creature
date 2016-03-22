{-# LANGUAGE OverloadedStrings #-}

module Main where

import App
import AppConfig (AppConfig(..), readConfig)
import Async.Job as Job
import CommonCreatures (WithErr)
import Control.Concurrent (threadDelay)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Reader
import qualified Data.Aeson as Aeson
import Features.Feature (FeatureFile, findFeatureFiles)
import Features.SearchableFeature (createFeaturesIndex)
import qualified Indexer
import qualified Network.AMQP as AMQP
import Network.AMQP.MessageBus as MB
import Products.CodeRepository (CodeRepository(..), codeRepositoryDir)
import Products.Product (ProductID)
import Retry (withRetry)

main :: IO ()
main = do
  appConfig <- readConfig
  withRetry (createFeaturesIndex (getElasticSearchConfig appConfig))
  runReaderT processJobs appConfig

processJobs :: App ()
processJobs =
  forever $ do
    (liftIO $ threadDelay 10000) >> ask >>= \cfg ->
      (liftIO $ MB.withConn (getRabbitMQConfig cfg) subscribeToProductCreation)
        >> (liftIO $ MB.withConn  (getRabbitMQConfig cfg) (getTopicMessages productCreatedQueue (MB.MessageHandler (indexProductFeatures cfg))))
        >>= return

subscribeToProductCreation :: MB.WithConn ()
subscribeToProductCreation = MB.subscribe productCreatedQueue productCreatedTopic

productCreatedTopic :: MB.TopicName
productCreatedTopic = (MB.TopicName "*.product.created")

productCreatedQueue :: MB.QueueName
productCreatedQueue = (MB.QueueName "product.created")

indexProductFeatures :: AppConfig -> (AMQP.Message, AMQP.Envelope) -> IO ()
indexProductFeatures cfg (message, envelope) =
  indexProductFeatures' cfg (parseJob message) envelope

indexProductFeatures' :: AppConfig -> Either String (Job CodeRepository) -> AMQP.Envelope -> IO ()
indexProductFeatures' _ (Left err) _ = putStrLn ("Unable to parse: " ++ err)
indexProductFeatures' cfg (Right (Job _ codeRepository)) envelope =
  let productID = getProductID codeRepository
  in (runExceptT $ featureFiles productID cfg)
       >>= (\files -> runExceptT $ indexFeatures files productID cfg)
       >>= (\result -> resolveJob result envelope)

featureFiles :: ProductID -> AppConfig -> WithErr [FeatureFile]
featureFiles productID cfg =
  findFeatureFiles (codeRepositoryDir productID (getGitConfig cfg))

indexFeatures :: Either String [FeatureFile] -> ProductID -> AppConfig -> WithErr ()
indexFeatures (Left err) _ _ = throwError err
indexFeatures (Right features) productID cfg =
  (liftIO $ Indexer.indexFeatures features productID (getGitConfig cfg) (getElasticSearchConfig cfg))
    >>= return

resolveJob :: Either String () -> AMQP.Envelope -> IO ()
resolveJob (Left err) _ = putStrLn ("Job failed: " ++ err)
resolveJob (Right _) envelope = MB.ackEnvelope envelope

parseJob :: AMQP.Message -> Either String (Job CodeRepository)
parseJob message = Aeson.eitherDecode (AMQP.msgBody message)

