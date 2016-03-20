{-# LANGUAGE OverloadedStrings #-}

module Network.AMQP.Internal.Consumer
( subscribe
, getTopicMessages
) where

import Config.Config (RabbitMQConfig (..))
import Control.Monad.Reader
import qualified Network.AMQP as AMQP
import Network.AMQP.Internal.Connection (withChannel)
import Network.AMQP.Internal.Types

subscribe :: Topic -> Queue -> WithAMQP ()
subscribe topic queue = ask >>= \cfg ->
  withChannel (\ch -> liftIO $ subscribeToTopic ch queue topic cfg)

subscribeToTopic :: AMQP.Channel -> Queue -> Topic -> RabbitMQConfig -> IO ()
subscribeToTopic ch (Queue q) (Topic topic) cfg =
  liftIO $ AMQP.bindQueue ch q (getExchangeName cfg) topic

getTopicMessages :: Queue -> MessageHandler -> WithAMQP ()
getTopicMessages (Queue q) (MessageHandler handler) =
  withChannel (\ch ->
    (liftIO $ AMQP.consumeMsgs ch q AMQP.Ack handler) >> return ())

