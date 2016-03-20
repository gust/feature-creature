{-# LANGUAGE OverloadedStrings #-}

module Network.AMQP.Internal.Producer
( produceTopicMessage
) where

import Network.AMQP.Internal.Connection (withChannel)
import Network.AMQP.Internal.Types
import qualified Network.AMQP as AMQP
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as Text

produceTopicMessage :: ExchangeName -> Topic -> Message -> WithAMQP ()
produceTopicMessage exchName topic msg =
  withChannel (\ch -> publishMessage ch exchName topic (buildMessage msg))

buildMessage :: Message -> AMQP.Message
buildMessage (Message msg) =
  AMQP.newMsg { AMQP.msgBody = BL.pack . Text.unpack $ msg
              , AMQP.msgDeliveryMode = Just AMQP.NonPersistent
              }

publishMessage :: AMQP.Channel -> ExchangeName -> Topic -> AMQP.Message -> IO ()
publishMessage channel (ExchangeName exchName) (Topic topic) message =
  AMQP.publishMsg channel exchName topic message

