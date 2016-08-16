module Network.AMQP.Internal.Connection
( createExchange
, deleteExchange
, createQueue
, deleteQueue
, withConn
, openConnection
, closeConnection
, openChannel
) where

import Network.AMQP.Config (RabbitMQConfig (..))
import Control.Exception (bracket)
import Control.Monad.Reader
import Data.Map as M
import qualified Data.Text as Text
import Data.Word (Word32)
import qualified Network.AMQP       as AMQP
import qualified Network.AMQP.Types as AMQP
import Network.AMQP.Internal.Types

withConn :: RabbitMQConfig -> WithConn a -> IO a
withConn cfg f =
  bracket (openConnection cfg) closeConnection $ \conn ->
    openChannel conn >>= \ch ->
    runReaderT (runConn f) (Connection conn ch cfg)

createExchange :: Exchange -> WithConn ()
createExchange (Exchange exchName exchType exchIsDurable) =
  (reader getChannel) >>= \ch ->
    let exchange = AMQP.newExchange { AMQP.exchangeName = exchName
                                    , AMQP.exchangeType = exchType
                                    , AMQP.exchangeDurable = exchIsDurable
                                    }
    in liftIO $ AMQP.declareExchange ch exchange

deleteExchange :: ExchangeName -> WithConn ()
deleteExchange (ExchangeName exchName) =
  (reader getChannel) >>= \ch ->
    liftIO $ AMQP.deleteExchange ch exchName

createQueue :: Queue -> WithConn QueueStatus
createQueue (Queue qName qAutoDelete qIsDurable) =
  (reader getChannel) >>= \ch ->
    let queue = AMQP.QueueOpts { AMQP.queueName = qName
                               , AMQP.queueAutoDelete = qAutoDelete
                               , AMQP.queueDurable = qIsDurable
                               , AMQP.queuePassive = False
                               , AMQP.queueExclusive = False
                               , AMQP.queueHeaders = (AMQP.FieldTable M.empty)
                               }
    in QueueStatus <$> (liftIO $ AMQP.declareQueue ch queue)

deleteQueue :: QueueName -> WithConn Word32
deleteQueue (QueueName queueName) =
  (reader getChannel) >>= \ch ->
    liftIO $ AMQP.deleteQueue ch queueName

openChannel :: AMQP.Connection -> IO AMQP.Channel
openChannel = AMQP.openChannel

openConnection :: RabbitMQConfig -> IO AMQP.Connection
openConnection cfg =
  AMQP.openConnection
    (Text.unpack $ getHostConfig cfg)
    (getPathConfig cfg)
    (getUserConfig cfg)
    (getPassConfig cfg)

closeConnection :: AMQP.Connection -> IO ()
closeConnection conn =
  AMQP.closeConnection conn
