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

import Config.Config (RabbitMQConfig (..))
import Control.Concurrent (threadDelay)
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
    (openChannel conn)
      >>= \ch -> runReaderT (runConn f) (Connection conn ch cfg)
      >>= \a  -> threadDelay (100 * 1000) -- the connection closes prematurely without this pause. not sure why yet.
      >> return a

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
  (putStrLn "") >>
  (putStrLn "--------------------- Opening AMQP connection... ---------------------") >>
  AMQP.openConnection
    (Text.unpack $ getHost cfg)
    (getPath cfg)
    (getUser cfg)
    (getPass cfg)

closeConnection :: AMQP.Connection -> IO ()
closeConnection conn =
  (putStrLn "--------------------- Closing AMQP connection... ---------------------") >>
    (putStrLn "") >>
    (AMQP.closeConnection conn)
