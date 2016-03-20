{-# LANGUAGE OverloadedStrings #-}

module Network.AMQP.Internal.Producer
( produceTopicMessage
) where

import Network.AMQP.Internal.Connection
import Network.AMQP.Internal.Types
import qualified Network.AMQP as AMQP
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as Text

produceTopicMessage :: ExchangeName -> RoutingKey -> Message -> WithAMQP ()
produceTopicMessage exchName rtKey msg =
  withChannel (\ch -> publishMessage ch exchName rtKey (buildMessage msg))

buildMessage :: Message -> AMQP.Message
buildMessage (Message msg) =
  AMQP.newMsg { AMQP.msgBody = BL.pack . Text.unpack $ msg
              , AMQP.msgDeliveryMode = Just AMQP.NonPersistent
              }

publishMessage :: AMQP.Channel -> ExchangeName -> RoutingKey -> AMQP.Message -> IO ()
publishMessage channel (ExchangeName exchName) (RoutingKey rtKey) message =
  AMQP.publishMsg channel exchName rtKey message

