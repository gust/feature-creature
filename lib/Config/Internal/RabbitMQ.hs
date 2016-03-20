module Config.Internal.RabbitMQ
( RabbitMQConfig (..)
, readRabbitMQConfig
) where

import System.Environment (getEnv)

data RabbitMQConfig =
  RabbitMQConfig { getHost :: String
                 , getPath :: String
                 , getUser :: String
                 , getPass :: String
                 }

readRabbitMQConfig :: IO RabbitMQConfig
readRabbitMQConfig =
  RabbitMQConfig
    <$> getEnv "FC_RABBITMQ_HOST"
    <*> getEnv "FC_RABBITMQ_PATH"
    <*> getEnv "FC_RABBITMQ_USER"
    <*> getEnv "FC_RABBITMQ_PASS"
