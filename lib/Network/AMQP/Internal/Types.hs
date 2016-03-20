{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.AMQP.Internal.Types
( WithAMQP (..)
, ExchangeName (..)
, Queue (..)
, Topic (..)
, Message (..)
, MessageHandler (..)
) where

import Config.Config (RabbitMQConfig (..))
import Control.Monad.Reader
import qualified Data.Text as Text
import qualified Network.AMQP as AMQP

newtype WithAMQP a   = WithAMQP { runAMQP :: ReaderT RabbitMQConfig IO a }
                         deriving (Functor, Applicative, Monad, MonadReader RabbitMQConfig, MonadIO)

newtype ExchangeName = ExchangeName Text.Text
newtype Queue        = Queue Text.Text
newtype Topic        = Topic Text.Text
newtype Message      = Message Text.Text

newtype MessageHandler = MessageHandler { processMessage :: (AMQP.Message, AMQP.Envelope) -> IO () }

