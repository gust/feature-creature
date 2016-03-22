{-# LANGUAGE OverloadedStrings #-}

module Products.Messaging
( MessageSource (..)
, createProductsQueue
, productCreatedTopic
, productsQueue
, subscribeToProductCreation
) where

import Async.Job as Job
import Control.Monad.Reader
import Network.AMQP.MessageBus as MB

data MessageSource = All
                   | API
  deriving (Show, Eq)

createProductsQueue :: MB.WithConn MB.QueueStatus
createProductsQueue =
  let queue = MB.Queue "products" False True
  in MB.createQueue queue

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
