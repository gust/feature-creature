{-# LANGUAGE OverloadedStrings #-}

module Network.AMQP.MessageBusSpec where

import Config.Config as Config (RabbitMQConfig (..))
import Control.Concurrent (threadDelay)
import Control.Monad.Reader
import Network.AMQP.MessageBus as MB
import qualified Network.AMQP as AMQP
import Test.Hspec

main :: IO ()
main = hspec spec

testConfig :: RabbitMQConfig
testConfig = RabbitMQConfig "localhost" "/" "guest" "guest" "test.exchange"

messageHandler :: MessageHandler
messageHandler = MessageHandler messageHandlerFn

messageHandlerFn :: (AMQP.Message, AMQP.Envelope) -> IO ()
messageHandlerFn (message, envelope) =
  putStrLn (show message) >> ackEnvelope envelope

testExchange :: Exchange
testExchange = Exchange (Config.getExchangeName testConfig) "topic" True

testQueue :: Queue
testQueue = Queue "test.queue" False True

assertMessageCount :: Int -> IO ()
assertMessageCount count =
  (runAMQP' (createQueue testQueue))
    >>= \(QueueStatus (_, msgCount, _)) -> msgCount `shouldBe` count

doSetup :: IO ()
doSetup =
  (runAMQP' (deleteExchange (ExchangeName $ MB.getExchangeName testExchange)))
    >> (runAMQP' (deleteQueue (QueueName $ MB.getQueueName testQueue)))
    >> (runAMQP' (createExchange testExchange))
    >> (runAMQP' (createQueue testQueue))
    >> return ()

spec :: Spec
spec = before_ doSetup $ do
  describe "Reading and Writing a Topical Message" $ do
    it "Reads a message for a given topic" $ do
      (runAMQP' (subscribe (QueueName "test.queue") (TopicName "test.important.things")))
        >> threadDelay (1000 * 100)

        >> assertMessageCount 0
        >> (runAMQP' (produceTopicMessage (TopicName "test.important.things") (Message "catapults!")))

        >> threadDelay (1000 * 100)
        >> assertMessageCount 1
        >> (runAMQP' (produceTopicMessage (TopicName "test.unimportant.things") (Message "haircuts")))

        >> threadDelay (1000 * 100)
        >> assertMessageCount 1
        >> (runAMQP' (getTopicMessages (QueueName "test.queue") messageHandler))

        >> threadDelay (1000 * 100)
        >> assertMessageCount 0
        >> return ()

-- runAMQP' is a shorter equivalent to (runReaderT (runAMQP someIOFn) testConfig)
runAMQP' = (flip runReaderT testConfig) . runAMQP
