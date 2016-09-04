{-# LANGUAGE OverloadedStrings #-}

module Network.AMQP.Config
( RabbitMQConfig (..)
, readRabbitMQConfig
) where

import Data.Text (Text, pack)
import System.Environment (getEnv)

data RabbitMQConfig =
  RabbitMQConfig { getHostConfig :: Text
                 , getPathConfig :: Text
                 , getUserConfig :: Text
                 , getPassConfig :: Text
                 , getExchangeNameConfig :: Text
                 } deriving (Show, Read, Eq)

readRabbitMQConfig :: IO RabbitMQConfig
readRabbitMQConfig =
  RabbitMQConfig
    <$> getEnvAsText "FC_RABBITMQ_HOST"
    <*> getEnvAsText "FC_RABBITMQ_PATH"
    <*> getEnvAsText "FC_RABBITMQ_USER"
    <*> getEnvAsText "FC_RABBITMQ_PASS"
    <*> getEnvAsText "FC_RABBITMQ_EXCHANGE_NAME"

getEnvAsText :: String -> IO Text
getEnvAsText varName = pack <$> (getEnv varName)
