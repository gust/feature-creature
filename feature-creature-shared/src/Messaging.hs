module Messaging
( Job (..)
, JobType (..)
, ProductCreatedJob (..)
, createTopicExchange
, createProductsQueue
, productCreatedTopic
, productsQueue
, productsQueueName
) where

import Data.Aeson as Aeson
import GHC.Generics (Generic)
import qualified Network.AMQP.Config as Config
import qualified Network.AMQP.MessageBus as MB
import Products.Api
import Repositories

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

data ProductCreatedJob = ProductCreatedJob Product RepositoryForm
  deriving (Show, Eq, Generic)

instance ToJSON ProductCreatedJob
instance FromJSON ProductCreatedJob

createTopicExchange :: Config.RabbitMQConfig -> MB.WithConn ()
createTopicExchange cfg =
  let exch = MB.Exchange (Config.getExchangeNameConfig cfg) "topic" True
  in MB.createExchange exch

createProductsQueue :: MB.WithConn MB.QueueStatus
createProductsQueue = MB.createQueue productsQueue

productsQueue :: MB.Queue
productsQueue = MB.Queue "products" False True

productsQueueName :: MB.QueueName
productsQueueName = MB.QueueName "products"

productCreatedTopic :: MB.TopicName
productCreatedTopic = MB.TopicName "feature_creature.product.created"
