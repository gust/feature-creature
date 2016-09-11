module App where

import Config.AppConfig
import Control.Monad.Reader
import qualified Network.AMQP.MessageBus as AMQP

type App a = ReaderT AppConfig IO a

withAMQPConn :: AMQP.WithConn a -> App a
withAMQPConn fn = reader getRabbitMQConfig >>= \cfg ->
  liftIO $ AMQP.withConn cfg fn

