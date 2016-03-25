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
import System.Environment                   (lookupEnv)

data AppConfig =
  AppConfig { getEnv                 :: Environment
            , getDBConfig            :: DBConfig
            , getElasticSearchConfig :: ElasticSearchConfig
            , getGitConfig           :: GitConfig
            , getRabbitMQConfig      :: RabbitMQConfig
            }

getAppConfig :: IO AppConfig
getAppConfig = do
  env            <- lookupSetting "ENV" Development
  dbPool         <- makePool env
  gitConfig      <- readGitConfig
  searchConfig   <- readElasticSearchConfig
  rabbitMQConfig <- readRabbitMQConfig
  return $ AppConfig { getEnv                 = env
                     , getDBConfig            = DBConfig dbPool
                     , getElasticSearchConfig = searchConfig
                     , getGitConfig           = gitConfig
                     , getRabbitMQConfig      = rabbitMQConfig
                     }

lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
    p <- lookupEnv env
    return $ case p of Nothing -> def
                       Just a  -> read a
