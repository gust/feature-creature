module Network.AMQP.MessageBus
( WithAMQP
, ExchangeName
, RoutingKey
, Message
, produceTopicMessage
) where

import Network.AMQP.Internal.Producer
import Network.AMQP.Internal.Types
