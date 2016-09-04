module Config.AppConfig
  ( AppConfig (..)
  , getAppConfig
  ) where

import Config.Environment (Environment (..))
import Config.RabbitMQ (RabbitMQConfig, readRabbitMQConfig)
import Data.Text as T
import LoadEnv
import System.Directory (getAppUserDataDirectory)
import System.FilePath.Posix ((</>), (<.>))

data AppConfig =
  AppConfig { getAppDataDirectory :: FilePath
            , getAppName          :: AppName
            , getEnv              :: Environment
            , getRabbitMQConfig   :: RabbitMQConfig
            }

type AppName = Text

getAppConfig :: AppName -> Environment -> IO AppConfig
getAppConfig appName env = do
  dataDirectory <- loadEnvVars appName env
  rabbitMQConfig <- readRabbitMQConfig
  return $ AppConfig
    { getAppDataDirectory = dataDirectory
    , getAppName          = appName
    , getEnv              = env
    , getRabbitMQConfig   = rabbitMQConfig
    }

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
