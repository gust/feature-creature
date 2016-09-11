module Main where

import App as App
import Config.AppConfig as App
import Config.Environment (getCurrentEnvironment)
import Control.Monad.Reader
import Data.Text as T
import qualified Messaging as M
import qualified Network.AMQP.MessageBus as MB
import qualified System.Environment as Env

main :: IO ()
main = do
  env      <- getCurrentEnvironment
  appName  <- T.pack <$> Env.getEnv "APP_NAME"

  putStrLn $ "\nLoading " ++ show env ++ " " ++ show appName ++ " configuration..."
  cfg <- App.getAppConfig appName env

  putStrLn (show cfg)

  putStrLn "\nEnsuring message queues are created..."
  let rabbitMQConfig = App.getRabbitMQConfig cfg
  _ <- MB.withConn rabbitMQConfig $
        M.createTopicExchange rabbitMQConfig >> M.createProductsQueue

  runReaderT (App.withAMQPConn (initMessageBroker cfg)) cfg
  {- runReaderT listenForIncomingMessages appConfig -}

initMessageBroker :: App.AppConfig -> MB.WithConn ()
initMessageBroker cfg = undefined
  {- MB.featureCreatureExchange (getRabbitMQConfig cfg) -}
    {- >> MB.createProductsQueue -}
    {- >> MB.subscribeToProductCreation -}

{- listenForIncomingMessages :: App () -}
{- listenForIncomingMessages = do -}
  {- cfg <- ask -}
  {- withAMQPConn $ do -}
    {- Msgs.getProductsMessages (MB.MessageHandler (messageReceivedCallback cfg)) -}
    {- liftIO $ putStrLn "I'm gonna sit here and run forever." -}
    {- liftIO $ putStrLn "Press any key to quit" -}
    {- liftIO $ getChar >> return () -}

{- messageReceivedCallback :: AppConfig -> (AMQP.Message, AMQP.Envelope) -> IO () -}
{- messageReceivedCallback cfg (message, envelope) = -}
  {- case parseProductCreatedJob message of -}
    {- (Left err) -> putStrLn err -}
    {- (Right (Job _ prodRepo)) -> -}
      {- runReaderT (pullRepo prodRepo) cfg -}
        {- >>= runExceptT -}
        {- >>= (resolveJob envelope) -}
        {- >> return () -}
