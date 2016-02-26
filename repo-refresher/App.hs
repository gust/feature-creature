module App where

import AppConfig
import Control.Monad.Reader

type App a = ReaderT AppConfig IO a
