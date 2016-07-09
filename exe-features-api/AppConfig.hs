module AppConfig
( AppConfig (..)
, Config.ElasticSearchConfig
, Config.Environment (..)
, Config.GitConfig (..)
, Config.RabbitMQConfig (..)
, getAppConfig
) where

import Config.Config as Config
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import Network.Wai                          (Middleware)
import qualified System.Environment as Env

data AppConfig =
  AppConfig { getEnv                 :: Environment
            , getRequestLogger       :: Middleware
            , getElasticSearchConfig :: ElasticSearchConfig
            , getGitConfig           :: GitConfig
            , getPort                :: Int
            , getRabbitMQConfig      :: RabbitMQConfig
            }

getAppConfig :: IO AppConfig
getAppConfig = do
  env            <- lookupSetting "ENV" Development
  gitConfig      <- readGitConfig
  searchConfig   <- readElasticSearchConfig
  rabbitMQConfig <- readRabbitMQConfig
  port           <- read <$> Env.getEnv "PORT"
  return $ AppConfig { getEnv                 = env
                     , getRequestLogger       = requestLogger env
                     , getElasticSearchConfig = searchConfig
                     , getGitConfig           = gitConfig
                     , getPort                = port
                     , getRabbitMQConfig      = rabbitMQConfig
                     }

requestLogger :: Environment -> Middleware
requestLogger Test        = id
requestLogger Development = logStdoutDev
requestLogger Production  = logStdout

lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
    p <- Env.lookupEnv env
    return $ case p of Nothing -> def
                       Just a  -> read a
