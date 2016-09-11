module Messaging
( Job (..)
, JobType (..)
, createTopicExchange
, createProductsQueue
) where

import qualified Network.AMQP.Config as Config
import qualified Network.AMQP.MessageBus as MB

import Data.Aeson as Aeson
import GHC.Generics (Generic)

data Job a =
  Job { getJobType :: JobType
      , getPayload :: a
      } deriving (Show, Generic)

instance ToJSON a   => ToJSON (Job a)
instance FromJSON a => FromJSON (Job a)

data JobType = ProductCreated
  deriving (Show, Read, Eq, Generic)

instance ToJSON JobType
instance FromJSON JobType

createTopicExchange :: Config.RabbitMQConfig -> MB.WithConn ()
createTopicExchange cfg =
  let exch = MB.Exchange (Config.getExchangeNameConfig cfg) "topic" True
  in MB.createExchange exch

createProductsQueue :: MB.WithConn MB.QueueStatus
createProductsQueue =
  let queue = MB.Queue "products" False True
  in MB.createQueue queue

