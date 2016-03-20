{-# LANGUAGE OverloadedStrings #-}

module Network.AMQP.MessageBusSpec where

import Config.Config (RabbitMQConfig (..))
import Network.AMQP.MessageBus

main :: IO ()
main = hspec spec

rabbitMQConfig :: RabbitMQConfig
rabbitMQConfig = RabbitMQConfig "localhost" "/" "guest" "guest"

spec :: Spec
spec =
  describe "Reading and Writing a Topical Message" $ do
    it "Reads a message for a given topic" $ do
      (runAMQP (produceTopicMessage testExch testRk testMsg) rabbitMQConfig)
        >> (runAMQP (consumeTopicMessages
