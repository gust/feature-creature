module Network.AMQP.Internal.Connection
( createExchange
, deleteExchange
, createQueue
, deleteQueue
, withChannel
) where

import Config.Config (RabbitMQConfig (..))
import Control.Exception (bracket)
import Control.Monad.Reader
import qualified Data.Text as Text
import Data.Word (Word32)
import qualified Network.AMQP as AMQP
import Network.AMQP.Internal.Types

withChannel :: (AMQP.Channel -> IO a) -> WithAMQP a
withChannel f = ask >>= \cfg ->
   liftIO $ withChannel' f cfg

withChannel' :: (AMQP.Channel -> IO a) -> RabbitMQConfig -> IO a
withChannel' f cfg =
  (openConnection cfg)
    >>= \conn -> (openChannel conn)
    >>= \ch -> f ch

{- withChannel' :: (AMQP.Channel -> IO a) -> RabbitMQConfig -> IO a -}
{- withChannel' f cfg = -}
  {- bracket (openConnection cfg) closeConnection $ \conn -> -}
    {- (openChannel conn) -}
      {- >>= \ch -> f ch -}
      {- >>= \result -> return result -}

createExchange :: Exchange -> WithAMQP ()
createExchange (Exchange exchName exchType exchIsDurable) = withChannel (\ch ->
  let exchange = AMQP.newExchange { AMQP.exchangeName = exchName
                                  , AMQP.exchangeType = exchType
                                  , AMQP.exchangeDurable = exchIsDurable
                                  }
  in AMQP.declareExchange ch exchange)

deleteExchange :: ExchangeName -> WithAMQP ()
deleteExchange (ExchangeName exchName) =
  withChannel (\ch -> AMQP.deleteExchange ch exchName)

createQueue :: Queue -> WithAMQP QueueStatus
createQueue (Queue qName qAutoDelete qIsDurable) = withChannel (\ch ->
  let queue = AMQP.newQueue { AMQP.queueName = qName
                            , AMQP.queueAutoDelete = qAutoDelete
                            , AMQP.queueDurable = qIsDurable
                            }
  in QueueStatus <$> AMQP.declareQueue ch queue)

deleteQueue :: QueueName -> WithAMQP Word32
deleteQueue (QueueName queueName) =
  withChannel (\ch -> AMQP.deleteQueue ch queueName)

openChannel :: AMQP.Connection -> IO AMQP.Channel
openChannel = AMQP.openChannel

openConnection :: RabbitMQConfig -> IO AMQP.Connection
openConnection cfg =
  AMQP.openConnection
    (Text.unpack $ getHost cfg)
    (getPath cfg)
    (getUser cfg)
    (getPass cfg)

closeConnection :: AMQP.Connection -> IO ()
closeConnection = AMQP.closeConnection
