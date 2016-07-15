module Config.AppConfig
  ( AppConfig (..)
  , ConfigException (..)
  , getAppConfig
  ) where

import Config.Environment (Environment (..))
import Control.Exception (Exception, throwIO)
import Control.Monad (liftM)
import Data.Maybe (fromJust)
import Data.Text as T
import Data.Typeable (Typeable)
import LoadEnv
import qualified Network.URL as URL
import qualified Network.Wai.Middleware.RequestLogger.LogEntries as LE
import System.Directory (getAppUserDataDirectory)
import qualified System.Environment as Env
import System.FilePath.Posix ((</>), (<.>))
import qualified Users.Api as UsersApi

type AppName = Text

data AppConfig = AppConfig
  { getAppName            :: AppName
  , getAppBasePath        :: Text
  , getAppDataDirectory   :: FilePath
  , getPort               :: Int
  , getEnv                :: Environment
  , getLogEntriesConfig   :: LE.Config
  , getAuthUrl            :: String
  , getUsersApiConfig     :: UsersApi.Config
  }

data ConfigException = ConfigException Text
    deriving (Show, Typeable)

instance Exception ConfigException

getAppConfig :: AppName -> Environment -> IO AppConfig
getAppConfig appName env = do
  dataDirectory <- loadEnvVars appName env
  port          <- Env.lookupEnv "PORT"
  basePath      <- T.pack <$> Env.getEnv "BASEPATH"
  leConfig      <- logEntriesConfig
  loginUrl      <- loadLoginUrl
  usersConfig   <- usersApiConfig
  let webServerPort = maybe 8080 id (liftM read port)

  return $ AppConfig
    { getAppName          = appName
    , getAppBasePath      = basePath
    , getAppDataDirectory = dataDirectory
    , getPort             = webServerPort
    , getEnv              = env
    , getLogEntriesConfig = leConfig
    , getAuthUrl          = loginUrl
    , getUsersApiConfig   = usersConfig
    }

  where
    loadLoginUrl :: IO String
    loadLoginUrl = do
      envUrl <- authURL
      case URL.importURL envUrl of
        Nothing -> throwIO $ ConfigException "Unable to parse APP_AUTH_BASE_PATH"
        (Just url) -> return $ URL.exportURL url

    authURL :: IO String
    authURL = Env.getEnv "APP_AUTH_BASE_PATH"

usersApiConfig :: IO UsersApi.Config
usersApiConfig = do
  basePath <- T.pack <$> Env.getEnv "APP_USERS_API_BASE_PATH"
  return $ UsersApi.Config basePath

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
loadEnvVars :: AppName -> Environment -> IO FilePath
loadEnvVars appName env = dataDirectory appName >>= \dataDir ->
  let filePath = dataDir </> envName env <.> "env"
  in loadEnvFrom filePath >> return dataDir
  where
    envName :: Environment -> FilePath
    envName = T.unpack . toLower . T.pack . show

    dataDirectory :: AppName -> IO FilePath
    dataDirectory = getAppUserDataDirectory . T.unpack

