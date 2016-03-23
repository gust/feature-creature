{-# LANGUAGE OverloadedStrings #-}

module Messaging.Products
( MessageSource (..)
, createProductsQueue
, getProductsMessages
, productCreatedTopic
, productsQueue
, subscribeToProductCreation
) where

import Control.Monad.Reader
import Network.AMQP.MessageBus as MB

data MessageSource = All
                   | API
  deriving (Show, Eq)

createProductsQueue :: MB.WithConn MB.QueueStatus
createProductsQueue =
  let queue = MB.Queue "products" False True
  in MB.createQueue queue

getProductsMessages :: MB.MessageHandler -> MB.WithConn ()
getProductsMessages handler = MB.getTopicMessages productsQueue handler

productsQueue :: MB.QueueName
productsQueue = MB.QueueName "products"

productCreatedTopic :: MessageSource -> MB.TopicName
productCreatedTopic All = MB.TopicName "api.product.created"
productCreatedTopic API = MB.TopicName "api.product.created"

subscribeToProductCreation :: MB.WithConn ()
subscribeToProductCreation =
  MB.subscribe productsQueue (productCreatedTopic All)
    >> createProductsQueue
    >>= liftIO . putStrLn . show

