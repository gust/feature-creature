{-# LANGUAGE OverloadedStrings #-}

module Network.AMQP.Internal.Consumer
( subscribe
, getTopicMessages
) where

import Config.Config as Config (RabbitMQConfig (..))
import Control.Monad.Reader
import qualified Network.AMQP as AMQP
import Network.AMQP.Internal.Connection (withChannel)
import Network.AMQP.Internal.Types

subscribe :: QueueName -> TopicName -> WithAMQP ()
subscribe queue topic = ask >>= \cfg ->
  withChannel (\ch -> liftIO $ subscribeToTopic ch queue topic cfg)

subscribeToTopic :: AMQP.Channel -> QueueName -> TopicName -> RabbitMQConfig -> IO ()
subscribeToTopic ch (QueueName q) (TopicName topic) cfg =
  liftIO $ AMQP.bindQueue ch q (Config.getExchangeName cfg) topic

getTopicMessages :: QueueName -> MessageHandler -> WithAMQP ()
getTopicMessages (QueueName q) (MessageHandler handler) =
  withChannel (\ch ->
    (liftIO $ AMQP.consumeMsgs ch q AMQP.Ack handler) >> return ())

