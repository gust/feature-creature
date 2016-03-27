module AppConfig
( AppConfig (..)
, Config.DBConfig (..)
, Config.ElasticSearchConfig
, Config.Environment (..)
, Config.GitConfig (..)
, Config.RabbitMQConfig (..)
, getAppConfig
) where

import Config.Config as Config
import Data.Text (Text, pack)
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import Network.Wai                          (Middleware)
import qualified System.Environment as Env  (lookupEnv, getEnv)

data AppConfig =
  AppConfig { getEnv                 :: Environment
            , getDBConfig            :: DBConfig
            , getRequestLogger       :: Middleware
            , getElasticSearchConfig :: ElasticSearchConfig
            , getGitConfig           :: GitConfig
            , getRabbitMQConfig      :: RabbitMQConfig
            , featuresAPI            :: Text
            }

getAppConfig :: IO AppConfig
getAppConfig = do
  env            <- lookupSetting "ENV" Development
  dbPool         <- makePool env
  gitConfig      <- readGitConfig
  searchConfig   <- readElasticSearchConfig
  rabbitMQConfig <- readRabbitMQConfig
  featuresAPIUrl <- Env.getEnv "FC_FEATURES_API"
  return $ AppConfig { getEnv                 = env
                     , getRequestLogger       = requestLogger env
                     , getDBConfig            = DBConfig dbPool
                     , getElasticSearchConfig = searchConfig
                     , getGitConfig           = gitConfig
                     , getRabbitMQConfig      = rabbitMQConfig
                     , featuresAPI            = (pack featuresAPIUrl)
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
