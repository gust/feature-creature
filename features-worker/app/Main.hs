module Main where

import App as App
import Config.AppConfig as App
import Config.Environment (getCurrentEnvironment)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader
import qualified Data.Aeson as Aeson
import Data.Int (Int64)
import Data.Text as T
import qualified Messaging as M
import qualified Network.AMQP as AMQP
import qualified Network.AMQP.MessageBus as MB
import qualified Products.Api as P
import qualified System.Environment as Env

main :: IO ()
main = do
  env      <- getCurrentEnvironment
  appName  <- T.pack <$> Env.getEnv "APP_NAME"

  putStrLn $ "\nLoading " ++ show env ++ " " ++ show appName ++ " configuration..."
  cfg <- App.getAppConfig appName env

  putStrLn "\nConfig loaded..."
  putStrLn (show cfg)

  putStrLn "\nEnsuring message queues are created..."
  let rabbitMQConfig = App.getRabbitMQConfig cfg
  _ <- MB.withConn rabbitMQConfig $
        M.createTopicExchange rabbitMQConfig >> M.createProductsQueue

  putStrLn "\nSubscribing to our topic..."
  _ <- MB.withConn rabbitMQConfig $
    MB.subscribe M.productsQueueName M.productCreatedTopic

  runReaderT listenForIncomingMessages cfg

listenForIncomingMessages :: App ()
listenForIncomingMessages = do
  cfg <- ask
  withAMQPConn $ do
    MB.getTopicMessages M.productsQueueName $
      MB.MessageHandler (messageReceivedCallback cfg)
    liftIO $ putStrLn "I'm gonna sit here and run forever."
    liftIO $ putStrLn "Press any key to quit"
    liftIO $ getChar >> return ()

messageReceivedCallback :: AppConfig -> (AMQP.Message, AMQP.Envelope) -> IO ()
messageReceivedCallback cfg (message, envelope) =
  case parseProductCreatedJob message of
    (Left err) -> putStrLn err
    (Right (M.Job _ prodRepo)) ->
      runReaderT (pullRepo prodRepo) cfg
        >>= runExceptT
        >>= (resolveJob envelope)
        >> return ()

parseProductCreatedJob :: AMQP.Message -> Either String (M.Job M.ProductCreatedJob)
parseProductCreatedJob message =
  Aeson.eitherDecode (AMQP.msgBody message)
    >>= filterJob

pullRepo :: M.ProductCreatedJob -> App (ExceptT String IO ())
pullRepo _ = undefined
{- pullRepo prodRepo = -}
  {- let prodId = parseProductId prodRepo -}
  {- in withDBPool (P.findProduct (toKey prodId)) >>= \prod -> -}
      {- case prod of -}
        {- Nothing  -> return $ throwError ("Product " ++ (show prodId) ++ " not found") -}
        {- (Just p) -> -}
          {- (pullProductRepo p prodId) -}
            {- >>= liftIO . runExceptT -}
            {- >>= \result -> updateRepoStatus result prodRepo -}

resolveJob :: AMQP.Envelope -> Either String a -> IO ()
resolveJob _ (Left err)       = putStrLn err
resolveJob envelope (Right _) = MB.ackEnvelope envelope

filterJob :: M.Job a -> Either String (M.Job a)
filterJob processableJob@(M.Job M.ProductCreated _) = Right processableJob

{- updateRepoStatus :: Either String String -> PR.ProductRepo -> App (WithErr ()) -}
{- updateRepoStatus (Left err) prodRepo = -}
  {- saveProductRepoStatus (toKey $ parseProductId prodRepo) Error (Just $ T.pack err) -}
{- updateRepoStatus (Right _) prodRepo = -}
  {- (withAMQPConn $ sendRepoCreatedMessage prodRepo) -}
    {- >> (saveProductRepoStatus (toKey $ parseProductId prodRepo) Ready Nothing) -}

{- pullProductRepo :: Product -> ProductID -> App (WithErr String) -}
{- pullProductRepo prod prodId = reader getGitConfig >>= \cfg -> -}
  {- return $ PR.updateRepo prod prodId cfg -}

{- saveProductRepoStatus :: ProductId -> RepositoryState -> Maybe Text -> App (WithErr ()) -}
{- saveProductRepoStatus prodId repoStatus errMsg = -}
  {- withDBPool $ P.updateProductRepoState prodId repoStatus errMsg -}

{- sendRepoCreatedMessage :: PR.ProductRepo -> WithConn () -}
{- sendRepoCreatedMessage prodRepo = -}
  {- MB.produceTopicMessage -}
    {- (Msgs.productRepoCreatedTopic Msgs.RepoPuller) -}
    {- (MB.Message (Job Job.RepositoryCreated prodRepo)) -}

parseProductId :: P.Product -> Int64
parseProductId = P.getID

