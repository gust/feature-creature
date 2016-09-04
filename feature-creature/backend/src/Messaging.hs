module Messaging
( createTopicExchange
, createProductsQueue
) where

import qualified Network.AMQP.Config as Config
import qualified Network.AMQP.MessageBus as MB

createTopicExchange :: Config.RabbitMQConfig -> MB.WithConn ()
createTopicExchange cfg =
  let exch = MB.Exchange (Config.getExchangeNameConfig cfg) "topic" True
  in MB.createExchange exch

createProductsQueue :: MB.WithConn MB.QueueStatus
createProductsQueue =
  let queue = MB.Queue "products" False True
  in MB.createQueue queue
