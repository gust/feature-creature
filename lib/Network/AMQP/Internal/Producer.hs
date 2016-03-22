{-# LANGUAGE OverloadedStrings #-}

module Network.AMQP.Internal.Producer
( ackEnvelope
, produceTopicMessage
) where

import Config.Config as Config (RabbitMQConfig (..))
import Control.Monad.Reader
import Data.Aeson as Aeson
import Network.AMQP.Internal.Types
import qualified Network.AMQP as AMQP

ackEnvelope :: AMQP.Envelope -> IO ()
ackEnvelope = AMQP.ackEnv

produceTopicMessage :: ToJSON a => TopicName -> Message a -> WithConn ()
produceTopicMessage topic msg = ask >>= \conn ->
  let exchangeName = ExchangeName $ Config.getExchangeName (getConfig conn)
      message      = buildMessage msg
      channel      = getChannel conn
  in liftIO $ publishMessage channel exchangeName topic message

buildMessage :: ToJSON a => Message a -> AMQP.Message
buildMessage (Message msg) =
  AMQP.newMsg { AMQP.msgBody = Aeson.encode msg
              , AMQP.msgDeliveryMode = Just AMQP.NonPersistent
              }

publishMessage :: AMQP.Channel -> ExchangeName -> TopicName -> AMQP.Message -> IO ()
publishMessage channel (ExchangeName exchName) (TopicName topic) message =
  AMQP.publishMsg channel exchName topic message

