module Config.AppConfig
  ( AppConfig (..)
  , getAppConfig
  ) where

import Auth0.Config as Auth0
import Config.Environment (Environment (..))
import Control.Monad (liftM)
import Data.Maybe (fromJust)
import Data.Text as T
import LoadEnv
import qualified Network.Wai.Middleware.RequestLogger.LogEntries as LE
import System.Directory (getAppUserDataDirectory)
import qualified System.Environment as Env
import System.FilePath.Posix ((</>), (<.>))
import qualified Users.Api as UsersApi

type AppName = Text

data AppConfig = AppConfig
  { getAppName            :: AppName
  , getPort               :: Int
  , getEnv                :: Environment
  , getLogEntriesConfig   :: LE.Config
  , getAuthConfig         :: Auth0.Config
  , getUsersApiConfig     :: UsersApi.Config
  } deriving (Show)

getAppConfig :: AppName -> Environment -> IO AppConfig
getAppConfig appName env = do
  loadEnvVars appName env
  port        <- Env.lookupEnv "PORT"
  leConfig    <- logEntriesConfig
  authConfig  <- auth0Config
  usersConfig <- usersApiConfig
  let webServerPort = maybe 8080 id (liftM read port)

  return $ AppConfig
    { getAppName          = appName
    , getPort             = webServerPort
    , getEnv              = env
    , getLogEntriesConfig = leConfig
    , getAuthConfig       = authConfig
    , getUsersApiConfig   = usersConfig
    }

usersApiConfig :: IO UsersApi.Config
usersApiConfig = do
  basePath <- T.pack <$> Env.getEnv "APP_USERS_API_BASE_PATH"
  return $ UsersApi.Config basePath

auth0Config :: IO Auth0.Config
auth0Config = do
  clientID     <- T.pack <$> Env.getEnv "APP_AUTH_ZERO_CLIENT_ID"
  clientSecret <- T.pack <$> Env.getEnv "APP_AUTH_ZERO_CLIENT_SECRET"
  redirectURI  <- T.pack <$> Env.getEnv "APP_AUTH_ZERO_REDIRECT_URI"
  grantType    <- T.pack <$> Env.getEnv "APP_AUTH_ZERO_GRANT_TYPE"
  basePath     <- T.pack <$> Env.getEnv "APP_AUTH_ZERO_BASE_PATH"

  return $ Auth0.Config
    clientID
    clientSecret
    redirectURI
    grantType
    basePath

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

