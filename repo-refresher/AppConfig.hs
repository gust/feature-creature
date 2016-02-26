module AppConfig
( AppConfig (..)
, DBConfig (..)
, readConfig
) where

import Config.Config
import System.Environment (lookupEnv)

data AppConfig =
  AppConfig { getEnv :: Environment
            , getDBConfig :: DBConfig
            }

readConfig :: IO AppConfig
readConfig = do
  env    <- lookupSetting "ENV" Development
  dbPool <- makePool env
  return $ AppConfig { getEnv = env
                     , getDBConfig = DBConfig dbPool
                     }

lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
    p <- lookupEnv env
    return $ case p of Nothing -> def
                       Just a  -> read a
