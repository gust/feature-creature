module Config.Config
( AWSConfig (..)
, DBConfig (..)
, ElasticSearchConfig (..)
, Environment (..)
, GitConfig (..)
, readAWSConfig
, readGitConfig
, readElasticSearchConfig
, makePool
) where

import System.Environment (getEnv, lookupEnv)

-- DB --

import Control.Monad.Logger        (runNoLoggingT, runStdoutLoggingT)
import Data.ByteString.Char8       (pack)
import Data.List                   (intersperse)
import Database.Persist.Postgresql (ConnectionPool, ConnectionString, createPostgresqlPool)

-- DB --

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

-------------------- Environment -----------------------

data Environment = Development
                 | Test
                 | Production
                 deriving (Eq, Show, Read)

-------------------- Environment -----------------------




-------------------- AWS -----------------------

readAWSConfig :: IO AWSConfig
readAWSConfig =
  AWSConfig
    <$> getEnv "FC_AWS_ACCESS_KEY"
    <*> getEnv "FC_AWS_SECRET_KEY"
    <*> getEnv "FC_AWS_SQS_URL"

-------------------- AWS -----------------------




-------------------- DB -----------------------

data DbConnectionString =
  DbConnectionString { dbname   :: String
                     , user     :: Maybe String
                     , password :: Maybe String
                     , host     :: Maybe String
                     , port     :: Maybe Int
                     } deriving (Show)

makePool :: Environment -> IO ConnectionPool
makePool Test = do
  connStr <- dbConnectionString
  runNoLoggingT $ createPostgresqlPool connStr 1
makePool _ = do
  connStr  <- dbConnectionString
  poolSize <- read <$> getEnv "FC_DB_POOL_SIZE"
  runStdoutLoggingT $ createPostgresqlPool connStr poolSize

dbConnectionString :: IO ConnectionString
dbConnectionString =
  (pack . toStr) <$> buildDBConnectionFromEnv

toStr :: DbConnectionString -> String
toStr cStr =
  concat . (intersperse " ")
  $ [ ("dbname=" ++ (dbname cStr))
    , (maybe "" (\x -> "user=" ++ x) (user cStr))
    , (maybe "" (\x -> "password=" ++ x) (password cStr))
    , (maybe "host=localhost" (\x -> "host=" ++ x) (host cStr))
    , (maybe "port=5432" (\x -> "port=" ++ show x) (port cStr))
    ]

buildDBConnectionFromEnv :: IO DbConnectionString
buildDBConnectionFromEnv =
  DbConnectionString
  <$> getEnv    "FC_DB_NAME"
  <*> lookupEnv "FC_DB_USER"
  <*> lookupEnv "FC_DB_PASS"
  <*> lookupEnv "FC_DB_HOST"
  <*> (parsePort <$> lookupEnv "FC_DB_PORT")

parsePort :: Maybe String -> Maybe Int
parsePort Nothing = Nothing
parsePort (Just p) = Just (read p)

-------------------- DB -----------------------




-------------------- Git -----------------------

readGitConfig :: IO GitConfig
readGitConfig =
  GitConfig
  <$> getEnv "FC_DATA_FILES_PATH"

-------------------- Git -----------------------




-------------------- ElasticSearch -----------------------

readElasticSearchConfig :: IO ElasticSearchConfig
readElasticSearchConfig =
  ElasticSearchConfig
    <$> getEnv "FC_ELASTIC_SEARCH_URL"
    <*> getEnv "FC_ELASTIC_SEARCH_INDEX_NAME"

-------------------- ElasticSearch -----------------------

