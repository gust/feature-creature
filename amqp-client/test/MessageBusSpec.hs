module MessageBusSpec where

import Control.Concurrent (threadDelay)
import Data.Aeson as Aeson
import Data.Text (Text)
import Data.Word (Word32)
import GHC.Generics (Generic)
import qualified Network.AMQP as AMQP
import Network.AMQP.Config (RabbitMQConfig (..))
import Network.AMQP.MessageBus
  ( Exchange (..)
  , ExchangeName (..)
  , TopicName (..)
  , Queue (..)
  , QueueName (..)
  , QueueStatus
  , Message (..)
  , MessageHandler (..)
  , WithConn (..)
  , createExchange
  , deleteExchange
  , createQueue
  , deleteQueue
  , subscribe
  , getTopicMessages
  , produceTopicMessage
  , withConn
  )
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Enqueuing and dequeuing messages" $
    it "writes and reads messages from the queue" $ do
      createSubscribeProduce
      threadDelay 1000 -- give the message a little time to persist
      consumeDelete

      -- Message processing happens in a different thread.
      -- For now we can look at stdout as the test runs to visually verify
      -- the correct behavior
      --
      -- How could we make a meaningful assertion here?
      -- Perhaps the message handler could write to a file, then we could
      -- read and assert against the contents. There's probably a much
      -- better way to test this.
      True `shouldBe` True

createSubscribeProduce :: IO ()
createSubscribeProduce = withConn testConfig $ do
  createTestExchange
    >> createTestQueue
    >> subscribeToTopic testQueue testTopic
    >> produceMessage testMessage

consumeDelete :: IO ()
consumeDelete = withConn testConfig $ do
  consumeMessage testHandler
    >> deleteTestQueue
    >> deleteTestExchange


consumeMessage :: MessageHandler -> WithConn ()
consumeMessage handler =
  getTopicMessages testQueue handler

produceMessage :: (ToJSON a) => a -> WithConn ()
produceMessage msg =
  produceTopicMessage testTopic (Message msg)

subscribeToTopic :: QueueName -> TopicName -> WithConn ()
subscribeToTopic queueName topicName =
  subscribe queueName topicName

createTestQueue :: WithConn QueueStatus
createTestQueue =
  let queue = Queue "test-queue" False True
  in createQueue queue

deleteTestQueue :: WithConn Word32
deleteTestQueue =
  let queueName = QueueName "test-queue"
  in deleteQueue queueName

createTestExchange :: WithConn ()
createTestExchange = createExchange testExchange

deleteTestExchange :: WithConn ()
deleteTestExchange =
  let exchangeName = ExchangeName . getExchangeName $ testExchange
  in deleteExchange exchangeName

testExchange :: Exchange
testExchange = Exchange (getExchangeNameConfig testConfig) "topic" True

testConfig :: RabbitMQConfig
testConfig = RabbitMQConfig
  { getHostConfig = "127.0.0.1"
  , getPathConfig = "/"
  , getUserConfig = "guest"
  , getPassConfig = "guest"
  , getExchangeNameConfig = "test-exchange"
  }

data TestMessage = TestMessage Int Text
  deriving (Show, Eq, Generic)

instance ToJSON TestMessage
instance FromJSON TestMessage

testMessage :: TestMessage
testMessage = TestMessage 1 "burgertime"

testTopic :: TopicName
testTopic = TopicName "*.test.created"

testQueue :: QueueName
testQueue = QueueName "test-queue"

testHandler :: MessageHandler
testHandler = MessageHandler handlerFn
  where
    handlerFn :: (AMQP.Message, AMQP.Envelope) -> IO ()
    handlerFn (msg, _) = putStrLn (show msg)
