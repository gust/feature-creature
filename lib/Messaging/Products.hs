{-# LANGUAGE OverloadedStrings #-}

module Messaging.Products
( MessageSource (..)
, createProductsQueue
, getProductsMessages
, productCreatedTopic
, productRepoCreatedTopic
, productsQueue
, subscribeToProductCreation
, subscribeToProductRepoCreation
) where

import Control.Monad.Reader
import Network.AMQP.MessageBus as MB

data MessageSource = All
                   | FeatureCreatureAPI
                   | RepoPuller
  deriving (Show, Eq)

createProductsQueue :: MB.WithConn MB.QueueStatus
createProductsQueue =
  let queue = MB.Queue "products" False True
  in MB.createQueue queue

getProductsMessages :: MB.MessageHandler -> MB.WithConn ()
getProductsMessages handler = MB.getTopicMessages productsQueue handler

subscribeToProductCreation :: MB.WithConn ()
subscribeToProductCreation =
  MB.subscribe productsQueue (productCreatedTopic All)
    >> createProductsQueue
    >>= liftIO . putStrLn . show

subscribeToProductRepoCreation :: MB.WithConn ()
subscribeToProductRepoCreation =
  MB.subscribe productsQueue (productRepoCreatedTopic All)
    >> createProductsQueue
    >>= liftIO . putStrLn . show

productsQueue :: MB.QueueName
productsQueue = MB.QueueName "products"

productCreatedTopic :: MessageSource -> MB.TopicName
productCreatedTopic All                = MB.TopicName "*.product.created"
productCreatedTopic FeatureCreatureAPI = MB.TopicName "fc_api.product.created"
productCreatedTopic RepoPuller         = MB.TopicName "repo_puller.product.created"

productRepoCreatedTopic :: MessageSource -> MB.TopicName
productRepoCreatedTopic All                = MB.TopicName "*.product.repo.created"
productRepoCreatedTopic FeatureCreatureAPI = MB.TopicName "fc_api.product.repo.created"
productRepoCreatedTopic RepoPuller         = MB.TopicName "repo_puller.product.repo.created"

