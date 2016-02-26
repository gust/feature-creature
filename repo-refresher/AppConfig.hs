module AppConfig
( AppConfig (..)
, DBConfig (..)
, GitConfig (..)
, readConfig
) where

import Config.Config
import System.Environment (lookupEnv)

data AppConfig =
  AppConfig { getEnv :: Environment
            , getDBConfig :: DBConfig
            , getGitConfig :: GitConfig
            }

readConfig :: IO AppConfig
readConfig = do
  env       <- lookupSetting "ENV" Development
  dbPool    <- makePool env
  gitConfig <- readGitConfig
  return $ AppConfig { getEnv = env
                     , getDBConfig = DBConfig dbPool
                     , getGitConfig = gitConfig
                     }

lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
    p <- lookupEnv env
    return $ case p of Nothing -> def
                       Just a  -> read a
