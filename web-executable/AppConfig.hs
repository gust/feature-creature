module AppConfig
( AppConfig (..)
, Config.AWSConfig
, Config.ElasticSearchConfig
, Config.Environment (..)
, Config.GitConfig (..)
, getAppConfig
) where

import Config.Config as Config
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import Network.Wai                          (Middleware)
import System.Environment                   (lookupEnv)

data AppConfig =
  AppConfig { getAWSConfig           :: AWSConfig
            , getEnv                 :: Environment
            , getDBConfig            :: DBConfig
            , getRequestLogger       :: Middleware
            , getElasticSearchConfig :: ElasticSearchConfig
            , getGitConfig           :: GitConfig
            }

getAppConfig :: IO AppConfig
getAppConfig = do
  env          <- lookupSetting "ENV" Development
  awsConfig    <- readAWSConfig
  dbPool       <- makePool env
  gitConfig    <- readGitConfig
  searchConfig <- readElasticSearchConfig
  return $ AppConfig { getAWSConfig           = awsConfig
                     , getEnv                 = env
                     , getRequestLogger       = requestLogger env
                     , getDBConfig            = DBConfig dbPool
                     , getElasticSearchConfig = searchConfig
                     , getGitConfig           = gitConfig
                     }

requestLogger :: Environment -> Middleware
requestLogger Test        = id
requestLogger Development = logStdoutDev
requestLogger Production  = logStdout

lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
    p <- lookupEnv env
    return $ case p of Nothing -> def
                       Just a  -> read a
