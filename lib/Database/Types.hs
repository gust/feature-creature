{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database.Types
( WithDBPool (..)
) where

import Control.Monad.Reader
import Database.Persist.Postgresql (ConnectionPool)

newtype WithDBPool a = WithDBPool { runPool :: ReaderT ConnectionPool IO a }
  deriving (Functor, Applicative, Monad, MonadReader ConnectionPool, MonadIO)
