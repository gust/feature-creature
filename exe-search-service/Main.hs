{-# LANGUAGE OverloadedStrings #-}

module Main where

import App
import AppConfig as Config (AppConfig(..), readConfig)
import Messaging.Job as Job
import CommonCreatures (WithErr)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Reader
import qualified Data.Aeson as Aeson
import Features.Feature (FeatureFile, findFeatureFiles)
import Features.SearchableFeature (createFeaturesIndex)
import qualified Indexer
import qualified Messaging.Exchanges as Msgs
import qualified Messaging.Products as Msgs
import qualified Network.AMQP as AMQP
import qualified Network.AMQP.MessageBus as MB
import Products.CodeRepository (CodeRepository(..), codeRepositoryDir)
import Products.Product (ProductID)
import Retry (withRetry)

main :: IO ()
main = do
  appConfig <- readConfig
  withRetry (createFeaturesIndex (getElasticSearchConfig appConfig))
  withRetry (MB.withConn (getRabbitMQConfig appConfig) (initMessageBroker appConfig))
  runReaderT processJobs appConfig

initMessageBroker :: AppConfig -> MB.WithConn ()
initMessageBroker cfg =
  Msgs.featureCreatureExchange (getRabbitMQConfig cfg)
    >> Msgs.createProductsQueue
    >> Msgs.subscribeToProductRepoCreation

processJobs :: App ()
processJobs = do
  cfg <- ask
  liftIO $ MB.withConn (getRabbitMQConfig cfg) $ do
    getMessages cfg
    liftIO $ putStrLn "I'm gonna sit here and run forever."
    liftIO $ putStrLn "Press any key to quit"
    liftIO $ getChar >> return ()

getMessages :: AppConfig -> MB.WithConn ()
getMessages cfg =
  Msgs.getProductsMessages (MB.MessageHandler (indexProductFeatures cfg))

indexProductFeatures :: AppConfig -> (AMQP.Message, AMQP.Envelope) -> IO ()
indexProductFeatures cfg (message, envelope) =
  (runExceptT $ indexProductFeatures' cfg (parseJob message))
    >>= (resolveJob envelope)
    >>= return

indexProductFeatures' :: AppConfig -> Either String (Job CodeRepository) -> WithErr ()
indexProductFeatures' _ (Left err) = throwError err
indexProductFeatures' cfg (Right (Job RepositoryCreated codeRepository)) =
  let productID = getProductID codeRepository
  in (liftIO $ putStrLn "Getting feature files...")
       >> (liftIO $ runExceptT $ featureFiles productID cfg)
       >>= (indexFeatures productID cfg)
indexProductFeatures' _ (Right (Job _ _)) = return ()

indexFeatures :: ProductID -> AppConfig -> Either String [FeatureFile] -> WithErr ()
indexFeatures _ _ (Left err) = (liftIO $ putStrLn ("Error: " ++ err)) >> throwError err
indexFeatures productID cfg (Right features) =
  (liftIO $ putStrLn ("Indexing features: " ++ (show features)))
    >> (liftIO $ Indexer.indexFeatures features productID (getGitConfig cfg) (getElasticSearchConfig cfg))
    >>= return

featureFiles :: ProductID -> AppConfig -> WithErr [FeatureFile]
featureFiles productID cfg =
  findFeatureFiles (codeRepositoryDir productID (getGitConfig cfg))

resolveJob :: AMQP.Envelope -> Either String () -> IO ()
resolveJob _ (Left err)       = putStrLn ("Job failed: " ++ err)
resolveJob envelope (Right _) = (putStrLn "Resolving envelope...") >> MB.ackEnvelope envelope

parseJob :: AMQP.Message -> Either String (Job CodeRepository)
parseJob message = Aeson.eitherDecode (AMQP.msgBody message)

