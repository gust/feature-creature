module Config 
  ( dbConnectionString
  , gitRepositoryStorePath
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
    _dbname   <- getEnv "FC_DB_NAME"
    _user     <- lookupEnv "FC_DB_USER"
    _password <- lookupEnv "FC_DB_PASS"
    _host     <- lookupEnv "FC_DB_HOST"
    _port     <- lookupEnv "FC_DB_PORT"

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

  gitRepositoryStorePath :: IO String
  gitRepositoryStorePath = getEnv "FC_DATA_FILES_PATH"

