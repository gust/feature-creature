module App
  ( AppT
  , AppConfig (..)
  , getAppConfig
  , runAppT
  ) where

import Config.AppConfig (AppConfig (..), getAppConfig)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Servant (Handler, ServantErr)

newtype AppT a = AppT { unApp :: ReaderT AppConfig Handler a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader AppConfig
           , MonadError ServantErr
           )
--
-- NOTE: This definition is equivalent and probably more idiomatic Servant.
--       However, I find the explicit ExceptT type clearer
-- newtype AppT a = AppT { unApp :: ReaderT AppConfig (ExceptT ServantErr IO) a }

runAppT :: AppConfig -> AppT a -> Handler a
runAppT cfg appT = runReaderT (unApp appT) cfg
