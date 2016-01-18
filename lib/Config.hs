module Config
( AWSConfig (..)
, DBConfig (..)
, ElasticSearchConfig (..)
, GitConfig (..)
) where

import Database.Persist.Postgresql (ConnectionPool)

data AWSConfig =
  AWSConfig { accessKey        :: String
            , secretKey        :: String
            , sqsUrl           :: String
            } deriving (Show)

data DBConfig =
  DBConfig { getPool :: ConnectionPool
           } deriving (Show)

data ElasticSearchConfig =
  ElasticSearchConfig { getESUrl     :: String
                      , getIndexName :: String
                      } deriving (Show)

data GitConfig =
  GitConfig { repoBasePath :: String
            } deriving (Show)
