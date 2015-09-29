module Config 
  ( dbConnectionString 
  ) where

  import Control.Applicative ((<$>))
  import Data.ByteString.Char8 (ByteString, pack)
  import Data.List (intersperse)
  import System.Environment (getEnv, lookupEnv)

  data DbConnectionString = DbConnectionString {
      dbname   :: String,
      user     :: Maybe String,
      password :: Maybe String,
      host     :: Maybe String,
      port     :: Maybe Int
    } deriving (Show)

  toStr :: DbConnectionString -> String
  toStr cStr = concat . (intersperse " ") $ [
    ("dbname=" ++ (dbname cStr))
    , (maybe "" (\x -> "user=" ++ x) (user cStr))
    , (maybe "" (\x -> "password=" ++ x) (password cStr))
    , (maybe "host=localhost" (\x -> "host=" ++ x) (host cStr))
    , (maybe "port=5432" (\x -> "port=" ++ show x) (port cStr))
    ]

  dbConnectionString :: IO ByteString
  dbConnectionString = (pack . toStr) <$> readDbConnectionString

  readDbConnectionString :: IO DbConnectionString
  readDbConnectionString = do
    _dbname   <- getEnv "DB_NAME"
    _user     <- lookupEnv "DB_USER"
    _password <- lookupEnv "DB_PASS"
    _host     <- lookupEnv "DB_HOST"
    _port     <- lookupEnv "DB_PORT"

    return DbConnectionString {
      dbname   = _dbname,
      user     = _user,
      password = _password,
      host     = _host,
      port     = parsePort _port
    }

  parsePort :: Maybe String -> Maybe Int
  parsePort Nothing = Nothing
  parsePort (Just p) = Just (read p)
