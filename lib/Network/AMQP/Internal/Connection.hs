module Network.AMQP.Internal.Connection
( createExchange
, withChannel
) where

import Config.Config (RabbitMQConfig (..))
import Control.Exception (IOException, bracket, handle)
import Control.Monad.Reader
import qualified Data.Text as Text
import qualified Network.AMQP as AMQP
import Network.AMQP.Internal.Types

withChannel :: (AMQP.Channel -> IO ()) -> WithAMQP ()
withChannel f = ask >>= \cfg ->
  liftIO $ handle handleIOException (liftIO $ withChannel' f cfg)

withChannel' :: (AMQP.Channel -> IO ()) -> RabbitMQConfig -> IO ()
withChannel' f cfg =
  bracket (openConnection cfg) closeConnection $ \conn ->
    (openChannel conn) >>= \ch -> f ch

createExchange :: AMQP.Channel -> AMQP.ExchangeOpts -> IO ()
createExchange = AMQP.declareExchange

openChannel :: AMQP.Connection -> IO AMQP.Channel
openChannel conn = AMQP.openChannel conn

openConnection :: RabbitMQConfig -> IO AMQP.Connection
openConnection cfg =
  liftIO $ AMQP.openConnection
    (Text.unpack $ getHost cfg)
    (getPath cfg)
    (getUser cfg)
    (getPass cfg)

closeConnection :: AMQP.Connection -> IO ()
closeConnection = AMQP.closeConnection

handleIOException :: IOException -> IO ()
handleIOException ex = putStrLn $ "IOExcpetion: " ++ (show ex)
