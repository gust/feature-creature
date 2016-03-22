{-# LANGUAGE OverloadedStrings #-}

module Main where

import App
import AppConfig as Config (AppConfig(..), RabbitMQConfig (..), readConfig)
import Async.Job as Job
import CommonCreatures (WithErr)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Reader
import qualified Data.Aeson as Aeson
import Features.Feature (FeatureFile, findFeatureFiles)
import Features.SearchableFeature (createFeaturesIndex)
import qualified Indexer
import qualified Network.AMQP as AMQP
import qualified Network.AMQP.MessageBus as MB
import Products.CodeRepository (CodeRepository(..), codeRepositoryDir)
import Products.Product (ProductID)
import qualified Products.Messaging as PM
import Retry (withRetry)

main :: IO ()
main = do
  appConfig <- readConfig
  withRetry (createFeaturesIndex (getElasticSearchConfig appConfig))
    >> MB.withConn (getRabbitMQConfig appConfig) (initMessageBroker appConfig)
    >> runReaderT processJobs appConfig

initMessageBroker :: AppConfig -> MB.WithConn ()
initMessageBroker cfg =
  (featureCreatureExchange (getRabbitMQConfig cfg))
    >> PM.createProductsQueue
    >>= (\queueStatus -> liftIO $ putStrLn ("Initialized queue: " ++ (show queueStatus)))
    >> PM.subscribeToProductCreation

processJobs :: App ()
processJobs =
  forever $ do
    (liftIO $ threadDelay (20000 * 100)) >> ask >>= \cfg ->
      (liftIO $ forkIO $ MB.withConn (getRabbitMQConfig cfg) (getMessages cfg))
        >> return ()

getMessages :: AppConfig -> MB.WithConn ()
getMessages cfg =
  (liftIO $ putStrLn "Getting messages from queue...")
    >> initMessageBroker cfg
    >> (MB.getTopicMessages PM.productsQueue (MB.MessageHandler (indexProductFeatures cfg)))

featureCreatureExchange :: RabbitMQConfig -> MB.WithConn ()
featureCreatureExchange cfg =
  let exch = MB.Exchange (Config.getExchangeName cfg) "topic" True
  in MB.createExchange exch

indexProductFeatures :: AppConfig -> (AMQP.Message, AMQP.Envelope) -> IO ()
indexProductFeatures cfg (message, envelope) =
  (runExceptT $ indexProductFeatures' cfg (parseJob message))
    >>= (resolveJob envelope)

indexProductFeatures' :: AppConfig -> Either String (Job CodeRepository) -> WithErr ()
indexProductFeatures' _ (Left err) = throwError err
indexProductFeatures' cfg (Right (Job _ codeRepository)) =
  let productID = getProductID codeRepository
  in (liftIO $ runExceptT $ featureFiles productID cfg)
       >>= (indexFeatures productID cfg)

indexFeatures :: ProductID -> AppConfig -> Either String [FeatureFile] -> WithErr ()
indexFeatures _ _ (Left err) = throwError err
indexFeatures productID cfg (Right features) =
  (liftIO $ Indexer.indexFeatures features productID (getGitConfig cfg) (getElasticSearchConfig cfg))
    >>= return

featureFiles :: ProductID -> AppConfig -> WithErr [FeatureFile]
featureFiles productID cfg =
  findFeatureFiles (codeRepositoryDir productID (getGitConfig cfg))

resolveJob :: AMQP.Envelope -> Either String () -> IO ()
resolveJob _ (Left err)       = putStrLn ("Job failed: " ++ err)
resolveJob envelope (Right _) = MB.ackEnvelope envelope

parseJob :: AMQP.Message -> Either String (Job CodeRepository)
parseJob message = Aeson.eitherDecode (AMQP.msgBody message)

