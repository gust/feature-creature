{-# LANGUAGE OverloadedStrings #-}

module Network.AMQP.Internal.Producer
( ackEnvelope
, produceTopicMessage
) where

import Config.Config as Config (RabbitMQConfig (..))
import Control.Monad.Reader
import Network.AMQP.Internal.Types
import qualified Network.AMQP as AMQP
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as Text

ackEnvelope :: AMQP.Envelope -> IO ()
ackEnvelope = AMQP.ackEnv

produceTopicMessage :: TopicName -> Message -> WithConn ()
produceTopicMessage topic msg = ask >>= \conn ->
  let exchangeName = ExchangeName $ Config.getExchangeName (getConfig conn)
      message      = buildMessage msg
      channel      = getChannel conn
  in liftIO $ publishMessage channel exchangeName topic message

buildMessage :: Message -> AMQP.Message
buildMessage (Message msg) =
  AMQP.newMsg { AMQP.msgBody = BL.pack . Text.unpack $ msg
              , AMQP.msgDeliveryMode = Just AMQP.NonPersistent
              }

publishMessage :: AMQP.Channel -> ExchangeName -> TopicName -> AMQP.Message -> IO ()
publishMessage channel (ExchangeName exchName) (TopicName topic) message =
  AMQP.publishMsg channel exchName topic message

