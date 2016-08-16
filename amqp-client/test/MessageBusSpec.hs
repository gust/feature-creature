module MessageBusSpec where

import Data.Word (Word32)
import Network.AMQP.Config (RabbitMQConfig (..))
import Network.AMQP.MessageBus
  ( Exchange (..)
  , ExchangeName (..)
  , TopicName (..)
  , Queue (..)
  , QueueName (..)
  , QueueStatus
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
      createTestExchange
        >> createTestQueue >>= putStrLn . show
        >> subscribeToTopic (QueueName "test-queue") (TopicName "*.test.created")
        -- TODO: produce a message
        -- TODO: consume that message
        >> deleteTestQueue >>= putStrLn . show
        >> deleteTestExchange
      True `shouldBe` True

subscribeToTopic :: QueueName -> TopicName -> IO ()
subscribeToTopic queueName topicName =
  withConn testConfig (subscribe queueName topicName)

createTestQueue :: IO QueueStatus
createTestQueue =
  let queue = Queue "test-queue" False True
  in withConn testConfig (createQueue queue)

deleteTestQueue :: IO Word32
deleteTestQueue =
  let queueName = QueueName "test-queue"
  in withConn testConfig (deleteQueue queueName)

createTestExchange :: IO ()
createTestExchange =
  withConn testConfig (createExchange testExchange)

deleteTestExchange :: IO ()
deleteTestExchange =
  let exchangeName = ExchangeName . getExchangeName $ testExchange
  in withConn testConfig (deleteExchange exchangeName)

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
