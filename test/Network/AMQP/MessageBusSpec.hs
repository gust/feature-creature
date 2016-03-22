{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Network.AMQP.MessageBusSpec where

import Config.Config as Config (RabbitMQConfig (..))
import Control.Concurrent (threadDelay)
import Control.Monad.Reader
import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.AMQP.MessageBus as MB
import qualified Network.AMQP as AMQP
import Test.Hspec

data TestMessage = TestMessage { getName :: Text, getAge :: Int }
                     deriving (Show, Generic)

instance ToJSON   TestMessage
instance FromJSON TestMessage

main :: IO ()
main = hspec spec

doSetup :: IO ()
doSetup = withConn testConfig resetBroker

spec :: Spec
spec = before_ doSetup $ do
  describe "Reading and Writing a Topical Message" $ do
    it "Reads a message for a given topic" $ do
      withConn testConfig testTopicSubscription

testTopicSubscription :: WithConn ()
testTopicSubscription =
  (subscribe (QueueName "test.queue") (TopicName "test.important.things"))
    >> (liftIO $ threadDelay (1000 * 100))

    >> (assertMessageCount 0)
    >> (produceTopicMessage (TopicName "test.important.things") (Message (TestMessage "catapults!" 800)))

    >> (liftIO $ threadDelay (1000 * 100))
    >> (assertMessageCount 1)
    >> (produceTopicMessage (TopicName "test.unimportant.things") (Message (TestMessage "haircuts" 25)))

    >> (liftIO $ threadDelay (1000 * 100))
    >> (assertMessageCount 1)
    >> (getTopicMessages (QueueName "test.queue") messageHandler)

    >> (liftIO $ threadDelay (1000 * 100))
    >> (assertMessageCount 0)
    >> return ()

assertMessageCount :: Int -> WithConn ()
assertMessageCount count =
  (createQueue testQueue)
    >>= \(QueueStatus (_, msgCount, _)) -> liftIO $ msgCount `shouldBe` count

resetBroker :: WithConn ()
resetBroker =
  deleteExchange (ExchangeName $ MB.getExchangeName testExchange)
    >> deleteQueue (QueueName $ MB.getQueueName testQueue)
    >> createExchange testExchange
    >> createQueue testQueue
    >> return ()

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
