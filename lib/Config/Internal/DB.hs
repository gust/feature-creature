
module Config.Internal.DB
( DBConfig (..)
, makePool
) where

import Config.Internal.Environment
import Control.Monad.Logger        (runNoLoggingT, runStdoutLoggingT)
import Data.ByteString.Char8       (pack)
import Data.List                   (intersperse)
import Database.Persist.Postgresql (ConnectionPool, ConnectionString, createPostgresqlPool)
import System.Environment (getEnv, lookupEnv)

data DBConfig =
  DBConfig { getPool :: ConnectionPool
           } deriving (Show)

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

