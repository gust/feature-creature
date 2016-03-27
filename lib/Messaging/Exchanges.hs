{-# LANGUAGE OverloadedStrings #-}

module Messaging.Exchanges
( featureCreatureExchange
) where

import qualified Config.Internal.RabbitMQ as Config
import qualified Network.AMQP.MessageBus as MB

featureCreatureExchange :: Config.RabbitMQConfig -> MB.WithConn ()
featureCreatureExchange cfg =
  let exch = MB.Exchange (Config.getExchangeName cfg) "topic" True
  in MB.createExchange exch

