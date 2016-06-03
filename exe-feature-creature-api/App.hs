module App
( App
) where

import AppConfig
import Control.Monad.Reader       (ReaderT)
import Control.Monad.Trans.Except (ExceptT)
import Servant                    (ServantErr (..))

type App =
  ReaderT AppConfig (ExceptT ServantErr IO)
