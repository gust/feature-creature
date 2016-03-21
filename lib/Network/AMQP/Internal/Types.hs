{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.AMQP.Internal.Types
( Exchange (..)
, ExchangeName (..)
, Message (..)
, MessageHandler (..)
, Queue (..)
, QueueName (..)
, QueueStatus (..)
, TopicName (..)
, WithAMQP (..)
) where

import Config.Config (RabbitMQConfig (..))
import Control.Monad.Reader
import Data.Text (Text)
import qualified Network.AMQP as AMQP

newtype WithAMQP a   = WithAMQP { runAMQP :: ReaderT RabbitMQConfig IO a }
                         deriving (Functor, Applicative, Monad, MonadReader RabbitMQConfig, MonadIO)

data Exchange =
  Exchange { getExchangeName :: Text
           , getExchangeType :: Text
           , getIsDurable    :: Bool
           }
data Queue =
  Queue { getQueueName       :: Text
        , getQueueAutoDelete :: Bool
        , getQueueIsDurable    :: Bool
        }
newtype QueueStatus  = QueueStatus (Text, MessageCount, ConsumerCount) deriving (Show, Read, Eq)
type MessageCount    = Int
type ConsumerCount   = Int

newtype ExchangeName = ExchangeName Text
newtype QueueName    = QueueName Text
newtype TopicName    = TopicName Text
newtype Message      = Message Text

newtype MessageHandler = MessageHandler { processMessage :: (AMQP.Message, AMQP.Envelope) -> IO () }

