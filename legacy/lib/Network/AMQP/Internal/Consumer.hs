{-# LANGUAGE OverloadedStrings #-}

module Network.AMQP.Internal.Consumer
( ackEnvelope
, subscribe
, getTopicMessages
) where

import Config.Config as Config (RabbitMQConfig (..))
import Control.Monad.Reader
import qualified Network.AMQP as AMQP
import Network.AMQP.Internal.Types

subscribe :: QueueName -> TopicName -> WithConn ()
subscribe (QueueName q) (TopicName t) =
  ask >>= \conn ->
    let ch = getChannel conn
        exchName = Config.getExchangeName (getConfig conn)
    in liftIO $ AMQP.bindQueue ch q exchName t

getTopicMessages :: QueueName -> MessageHandler -> WithConn ()
getTopicMessages (QueueName q) (MessageHandler handler) =
  ask >>= \conn ->
    let ch = getChannel conn
    in (liftIO $ AMQP.consumeMsgs ch q AMQP.Ack handler) >> return ()

ackEnvelope :: AMQP.Envelope -> IO ()
ackEnvelope = AMQP.ackEnv

