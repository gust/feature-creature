{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.AMQP.Internal.Types
( WithAMQP (..)
, ExchangeName (..)
, Topic (..)
, Message (..)
) where

import Config.Config (RabbitMQConfig (..))
import Control.Monad.Reader
import qualified Data.Text as Text

newtype WithAMQP a   = WithAMQP { runAMQP :: ReaderT RabbitMQConfig IO a }
                         deriving (Functor, Applicative, Monad, MonadReader RabbitMQConfig, MonadIO)

newtype ExchangeName = ExchangeName Text.Text
newtype Topic        = Topic Text.Text
newtype Message      = Message Text.Text

