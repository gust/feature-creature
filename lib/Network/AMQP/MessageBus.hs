module Network.AMQP.MessageBus
( WithAMQP
, ExchangeName
, Topic
, Message
, produceTopicMessage
) where

import Network.AMQP.Internal.Producer
import Network.AMQP.Internal.Types
