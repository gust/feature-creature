module Config.Environment
  ( Environment (..)
  , getCurrentEnvironment
  ) where

import System.Environment (lookupEnv)

data Environment = Development
                 | Test
                 | Production
  deriving (Eq, Show, Read)

getCurrentEnvironment :: IO Environment
getCurrentEnvironment = lookupSetting "ENV" Development

lookupSetting :: String -> Environment -> IO Environment
lookupSetting env defaultEnv = fmap (maybe defaultEnv read) (lookupEnv env)
