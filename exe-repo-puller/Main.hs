{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api.Types (APIProduct (..))
import App
import AppConfig as Config (AppConfig(..), DBConfig (..), RabbitMQConfig (..), getAppConfig)
import Messaging.Job as Job
import CommonCreatures (WithErr)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Reader
import Data.Aeson (ToJSON)
import qualified Data.Aeson as Aeson
import Data.Text as T (Text, pack)
import Database.Types (runPool)
import qualified Messaging.Products as MP
import Models
import ModelTypes (RepositoryState (Ready, Error))
import qualified Network.AMQP as AMQP
import Network.AMQP.MessageBus as MB
import qualified Products.CodeRepository as CR
import Products.Product (ProductID)
import qualified Products.Product as P
import Retry (withRetry)

main :: IO ()
main = getAppConfig >>= \appConfig ->
  withRetry (MB.withConn (getRabbitMQConfig appConfig) (initMessageBroker appConfig))
    >> runReaderT listenForIncomingMessages appConfig

initMessageBroker :: AppConfig -> MB.WithConn ()
initMessageBroker cfg =
  featureCreatureExchange (getRabbitMQConfig cfg)
    >> MP.createProductsQueue
    >> MP.subscribeToProductCreation

-- this is duplicated in search-service#Main
featureCreatureExchange :: RabbitMQConfig -> MB.WithConn ()
featureCreatureExchange cfg =
  let exch = MB.Exchange (Config.getExchangeName cfg) "topic" True
  in MB.createExchange exch

listenForIncomingMessages :: App ()
listenForIncomingMessages = do
  cfg <- ask
  liftIO $ MB.withConn (getRabbitMQConfig cfg) $ do
    processNewProductMessages cfg
    liftIO $ putStrLn "I'm gonna sit here and run forever."
    liftIO $ putStrLn "Press any key to quit"
    liftIO $ getChar >> return ()

processNewProductMessages :: AppConfig -> MB.WithConn ()
processNewProductMessages cfg = MP.getProductsMessages (MB.MessageHandler (messageReceivedCallback cfg))

messageReceivedCallback :: AppConfig -> (AMQP.Message, AMQP.Envelope) -> IO ()
messageReceivedCallback cfg (message, envelope) =
  runReaderT (pullRepo (parseJob message)) cfg
    >>= runExceptT
    >>= (resolveJob envelope)
    >> return ()

pullRepo :: Either String (Job APIProduct) -> App (WithErr ())
pullRepo (Left err) = return $ throwError err
pullRepo (Right (Job Job.ProductCreated apiProduct)) = ask >>= \cfg ->
  let prodId = maybe (-1) id (productID apiProduct)
  in (liftIO $ runReaderT (runPool (P.findProduct (toKey prodId))) (getPool . getDBConfig $ cfg)) >>= \prod ->
      case prod of
        Nothing  -> return $ throwError ("Product " ++ (show prodId) ++ " not found")
        (Just p) -> pullProductRepo p prodId
pullRepo (Right (Job jobType _)) = return $ throwError ("Ignoring job " ++ (show jobType))

pullProductRepo :: Product -> ProductID -> App (WithErr ())
pullProductRepo prod prodId = ask >>= \cfg ->
  (liftIO $ (runExceptT $ CR.updateRepo prod prodId (getGitConfig cfg))) >>= \result ->
    case result of
      (Left err) -> saveProductRepoStatus (toKey prodId) Error (Just $ T.pack err)
      (Right _)  -> saveProductRepoStatus (toKey prodId) Ready Nothing

saveProductRepoStatus :: ProductId -> RepositoryState -> Maybe Text -> App (WithErr ())
saveProductRepoStatus prodId repoStatus errMsg = ask >>= \cfg ->
  let dbCommand = P.updateProductRepoState prodId repoStatus errMsg
      pool      = getPool . getDBConfig $ cfg
  in liftIO (runReaderT (runPool dbCommand) pool)

resolveJob :: AMQP.Envelope -> Either String a -> IO ()
resolveJob _ (Left err)       = putStrLn err
resolveJob envelope (Right _) = (putStrLn "Job succeeded! Resolving envelope...") >> MB.ackEnvelope envelope

parseJob :: AMQP.Message -> Either String (Job APIProduct)
parseJob message = Aeson.eitherDecode (AMQP.msgBody message)

sendRepoCreatedMessage :: ToJSON a => Job a -> WithConn ()
sendRepoCreatedMessage job = MB.produceTopicMessage (MP.productRepoCreatedTopic MP.RepoPuller) (MB.Message job)
