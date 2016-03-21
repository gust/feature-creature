{-# LANGUAGE OverloadedStrings #-}

module Network.AMQP.Internal.Producer
( produceTopicMessage
, ackEnvelope
) where

import Config.Config as Config (RabbitMQConfig (..))
import Control.Monad.Reader
import Network.AMQP.Internal.Connection (withChannel)
import Network.AMQP.Internal.Types
import qualified Network.AMQP as AMQP
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as Text

produceTopicMessage :: TopicName -> Message -> WithAMQP ()
produceTopicMessage topic msg = ask >>= \cfg ->
  let exchangeName = ExchangeName $ Config.getExchangeName cfg
      message      = buildMessage msg
  in withChannel (\ch -> publishMessage ch exchangeName topic message)

buildMessage :: Message -> AMQP.Message
buildMessage (Message msg) =
  AMQP.newMsg { AMQP.msgBody = BL.pack . Text.unpack $ msg
              , AMQP.msgDeliveryMode = Just AMQP.NonPersistent
              }

publishMessage :: AMQP.Channel -> ExchangeName -> TopicName -> AMQP.Message -> IO ()
publishMessage channel (ExchangeName exchName) (TopicName topic) message =
  AMQP.publishMsg channel exchName topic message

ackEnvelope :: AMQP.Envelope -> IO ()
ackEnvelope = AMQP.ackEnv

