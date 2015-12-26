module AppConfig
( AppConfig (..)
, Environment (..)
, getAppConfig
) where

import qualified Config.AWS as AWS          (AWSConfig, getAWSConfig)
import Config.Database                      (makePool)
import Config.Environment                   (Environment (..))
import Config.Git                           (GitConfig (..), getGitConfig)
import Database.Persist.Postgresql          (ConnectionPool)
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import Network.Wai                          (Middleware)
import System.Environment                   (lookupEnv)

data AppConfig =
  AppConfig { getAWSConfig     :: AWS.AWSConfig
            , getEnv           :: Environment
            , getPool          :: ConnectionPool
            , getRequestLogger :: Middleware
            , gitConfig        :: GitConfig
            }

getAppConfig :: IO AppConfig
getAppConfig = do
  env       <- lookupSetting "ENV" Development
  awsConfig <- AWS.getAWSConfig
  dbPool    <- makePool env
  gitCfg    <- getGitConfig
  return $ AppConfig { getAWSConfig     = awsConfig
                     , getEnv           = env
                     , getRequestLogger = requestLogger env
                     , getPool          = dbPool
                     , gitConfig        = gitCfg
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
