module Config.Git
( GitConfig (..)
, getGitConfig
) where

import Config (GitConfig (..))
import System.Environment (getEnv)

getGitConfig :: IO GitConfig
getGitConfig =
  GitConfig
  <$> getEnv "FC_DATA_FILES_PATH"
