module App where

import Config
import Control.Monad.Reader

type App a = ReaderT AppConfig IO a
