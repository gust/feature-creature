{-# LANGUAGE ExistentialQuantification #-}

module Config.AppConfig
  ( AppConfig (..)
  , getAppConfig
  ) where

import Config.Internal.Database (ConnectionPool, makePool)
import Config.Environment (Environment (..))
import Control.Monad (liftM)
import Data.Maybe (fromJust)
import Data.Text as T
import Database.Types (DBAccess (..), db)
import LoadEnv
import qualified Network.Wai.Middleware.RequestLogger.LogEntries as LE
import System.Directory (getAppUserDataDirectory)
import qualified System.Environment as Env
import System.FilePath.Posix ((</>), (<.>))

type AppName = Text

data AppConfig = forall m. Monad m => AppConfig
  { getAppName            :: AppName
  , getPort               :: Int
  , getEnv                :: Environment
  , getDBConn             :: ConnectionPool
  , getDB                 :: DBAccess m
  , getLogEntriesConfig   :: LE.Config
  }

getAppConfig :: AppName -> Environment -> IO AppConfig
getAppConfig appName env = do
  loadEnvVars appName env
  port     <- Env.lookupEnv "PORT"
  dbPool   <- makePool env
  leConfig <- logEntriesConfig
  let webServerPort = maybe 8080 id (liftM read port)
  return $ AppConfig appName webServerPort env dbPool (db dbPool) leConfig

-- The unsafe call to :fromJust is acceptable here
-- since we are bootstrapping the application.
-- If required configuration is not present and parsible,
-- then we should fail to start the app
logEntriesConfig :: IO LE.Config
logEntriesConfig = do
  hostname <- Env.getEnv "APP_LOGENTRIES_DATA_DOMAIN"
  port     <- read <$> Env.getEnv "APP_LOGENTRIES_DATA_PORT"
  token    <- (fromJust . LE.fromString) <$> Env.getEnv "APP_LOGENTRIES_LOG_KEY"
  return $ LE.Config hostname port token

-- laodEnvVars will look for configuration files matching the lowercase
-- environment name in the user's data directory
-- Ex. if the app name is 'cool-app' and the environment is Production,
--     the env vars will be loaded from ~/.cool-app/production.env
-- loadEnvVars will NOT raise an exception if the environment file is not found
loadEnvVars :: AppName -> Environment -> IO ()
loadEnvVars appName env = dataDirectory appName >>= \dataDir -> do
  let filePath = dataDir </> envName env <.> "env"
  loadEnvFrom $ filePath
  where
    envName :: Environment -> FilePath
    envName = T.unpack . toLower . T.pack . show

    dataDirectory :: AppName -> IO FilePath
    dataDirectory = getAppUserDataDirectory . T.unpack

