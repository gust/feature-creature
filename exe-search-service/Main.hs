{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api.Types.Product (APIProduct (..))
import App
import AppConfig as Config (AppConfig(..), readConfig)
import Messaging.Job as Job
import CommonCreatures (WithErr)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Reader
import qualified Data.Aeson as Aeson
import Features.Feature as F (findFeatureFiles)
import Features.SearchableFeature (createFeaturesIndex)
import qualified Indexer
import qualified Messaging.Exchanges as Msgs
import qualified Messaging.Products as Msgs
import qualified Network.AMQP as AMQP
import qualified Network.AMQP.MessageBus as MB
import qualified Products.CodeRepository as CR
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
    Msgs.getProductsMessages (MB.MessageHandler (messageReceivedCallback cfg))
    liftIO $ putStrLn "I'm gonna sit here and run forever."
    liftIO $ putStrLn "Press any key to quit"
    liftIO $ getChar >> return ()

messageReceivedCallback :: AppConfig -> (AMQP.Message, AMQP.Envelope) -> IO ()
messageReceivedCallback cfg (message, envelope) =
  case parseRepoCreatedJob message of
    (Left err) -> putStrLn err
    (Right (Job _ apiProduct)) ->
      runReaderT (indexProductFeatures apiProduct) cfg
        >>= runExceptT
        >>= (resolveJob envelope)
        >>= return

indexProductFeatures :: APIProduct -> App (WithErr ())
indexProductFeatures prod =
  case productID prod of
    Nothing -> return $ throwError $ "Missing productID: " ++ (show prod)
    (Just prodID) -> indexFeatures prodID

indexFeatures :: ProductID -> App (WithErr ())
indexFeatures prodID = ask >>= \cfg -> do
  let gitConfig = getGitConfig cfg
  let esConfig  = getElasticSearchConfig cfg
  featureFiles <- liftIO $ runExceptT $ F.findFeatureFiles (CR.codeRepositoryDir prodID gitConfig)
  case featureFiles of
    (Left err) -> return $ throwError err
    (Right features) ->
      (liftIO $ putStrLn ("Indexing features: " ++ (show features)))
        >> (liftIO $ Indexer.indexFeatures features prodID gitConfig esConfig)
        >>= return . return

resolveJob :: AMQP.Envelope -> Either String () -> IO ()
resolveJob _ (Left err)       = putStrLn ("Job failed: " ++ err)
resolveJob envelope (Right _) = (putStrLn "Resolving envelope...") >> MB.ackEnvelope envelope

parseRepoCreatedJob :: AMQP.Message -> Either String (Job APIProduct)
parseRepoCreatedJob message =
  Aeson.eitherDecode (AMQP.msgBody message)
    >>= filterJob

filterJob :: Job APIProduct -> Either String (Job APIProduct)
filterJob processableJob@(Job Job.RepositoryCreated _) = Right processableJob
filterJob (Job jobType _) = Left $ "Ignoring job " ++ (show jobType)
