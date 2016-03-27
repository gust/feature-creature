module App where

import AppConfig
import Control.Monad.Reader
import qualified Database.Types as DB
import qualified Network.AMQP.MessageBus as AMQP

type App a = ReaderT AppConfig IO a

withDBPool :: DB.WithDBPool a -> App a
withDBPool query = reader getDBConfig >>= \cfg ->
  liftIO $ runReaderT (DB.runPool query) (getPool cfg)

withAMQPConn :: AMQP.WithConn a -> App a
withAMQPConn fn = reader getRabbitMQConfig >>= \cfg ->
  liftIO $ AMQP.withConn cfg fn


