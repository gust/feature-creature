module App
( App
) where

import AppConfig
import Control.Monad.Reader       (ReaderT)
import Control.Monad.Trans.Either (EitherT)
import Servant                    (ServantErr (..))

type App =
  ReaderT AppConfig (EitherT ServantErr IO)
