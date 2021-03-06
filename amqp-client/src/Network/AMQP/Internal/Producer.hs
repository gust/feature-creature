{-# LANGUAGE OverloadedStrings #-}

module Network.AMQP.Internal.Producer
( produceTopicMessage
) where

import Network.AMQP.Config as Config (RabbitMQConfig (..))
import Control.Monad.Reader
import Data.Aeson as Aeson
import Network.AMQP.Internal.Types
import qualified Network.AMQP as AMQP

produceTopicMessage :: ToJSON a => TopicName -> Message a -> WithConn ()
produceTopicMessage topic msg = ask >>= \conn ->
  let exchangeName = ExchangeName $ Config.getExchangeNameConfig (getConfig conn)
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

