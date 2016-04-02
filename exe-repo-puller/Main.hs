{-# LANGUAGE OverloadedStrings #-}

module Main where

import App (App, withAMQPConn, withDBPool)
import AppConfig as Config (AppConfig(..), getAppConfig)
import Messaging.Job as Job
import CommonCreatures (WithErr)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Reader
import qualified Data.Aeson as Aeson
import Data.Text as T (Text, pack)
import qualified Messaging.Exchanges as Msgs
import qualified Messaging.Products as Msgs
import Models
import ModelTypes (RepositoryState (Ready, Error))
import qualified Network.AMQP as AMQP
import Network.AMQP.MessageBus as MB
import Products.Product (ProductID)
import qualified Products.Product as P
import qualified Products.ProductRepo as PR
import Retry (withRetry)

main :: IO ()
main = do
  appConfig <- getAppConfig
  withRetry $ runReaderT (withAMQPConn (initMessageBroker appConfig)) appConfig
  runReaderT listenForIncomingMessages appConfig

initMessageBroker :: AppConfig -> MB.WithConn ()
initMessageBroker cfg =
  Msgs.featureCreatureExchange (getRabbitMQConfig cfg)
    >> Msgs.createProductsQueue
    >> Msgs.subscribeToProductCreation

listenForIncomingMessages :: App ()
listenForIncomingMessages = do
  cfg <- ask
  withAMQPConn $ do
    Msgs.getProductsMessages (MB.MessageHandler (messageReceivedCallback cfg))
    liftIO $ putStrLn "I'm gonna sit here and run forever."
    liftIO $ putStrLn "Press any key to quit"
    liftIO $ getChar >> return ()

messageReceivedCallback :: AppConfig -> (AMQP.Message, AMQP.Envelope) -> IO ()
messageReceivedCallback cfg (message, envelope) =
  case parseProductCreatedJob message of
    (Left err) -> putStrLn err
    (Right (Job _ prodRepo)) ->
      runReaderT (pullRepo prodRepo) cfg
        >>= runExceptT
        >>= (resolveJob envelope)
        >> return ()

pullRepo :: PR.ProductRepo -> App (WithErr ())
pullRepo prodRepo =
  let prodId = parseProductId prodRepo
  in withDBPool (P.findProduct (toKey prodId)) >>= \prod ->
      case prod of
        Nothing  -> return $ throwError ("Product " ++ (show prodId) ++ " not found")
        (Just p) ->
          (pullProductRepo p prodId)
            >>= liftIO . runExceptT
            >>= \result -> updateRepoStatus result prodRepo

updateRepoStatus :: Either String String -> PR.ProductRepo -> App (WithErr ())
updateRepoStatus (Left err) prodRepo =
  saveProductRepoStatus (toKey $ parseProductId prodRepo) Error (Just $ T.pack err)
updateRepoStatus (Right _) prodRepo =
  (withAMQPConn $ sendRepoCreatedMessage prodRepo)
    >> (saveProductRepoStatus (toKey $ parseProductId prodRepo) Ready Nothing)

pullProductRepo :: Product -> ProductID -> App (WithErr String)
pullProductRepo prod prodId = reader getGitConfig >>= \cfg ->
  return $ PR.updateRepo prod prodId cfg

saveProductRepoStatus :: ProductId -> RepositoryState -> Maybe Text -> App (WithErr ())
saveProductRepoStatus prodId repoStatus errMsg =
  withDBPool $ P.updateProductRepoState prodId repoStatus errMsg

resolveJob :: AMQP.Envelope -> Either String a -> IO ()
resolveJob _ (Left err)       = putStrLn err
resolveJob envelope (Right _) =
  putStrLn "Job succeeded! Resolving envelope..."
    >> MB.ackEnvelope envelope

parseProductCreatedJob :: AMQP.Message -> Either String (Job PR.ProductRepo)
parseProductCreatedJob message =
  Aeson.eitherDecode (AMQP.msgBody message)
    >>= filterJob

filterJob :: Job a -> Either String (Job a)
filterJob processableJob@(Job Job.ProductCreated _) = Right processableJob
filterJob (Job jobType _) = Left $ "Ignoring job " ++ (show jobType)

sendRepoCreatedMessage :: PR.ProductRepo -> WithConn ()
sendRepoCreatedMessage prodRepo =
  MB.produceTopicMessage
    (Msgs.productRepoCreatedTopic Msgs.RepoPuller)
    (MB.Message (Job Job.RepositoryCreated prodRepo))

parseProductId :: PR.ProductRepo -> P.ProductID
parseProductId prodRepo = maybe (-1) id (PR.getProductId prodRepo)

