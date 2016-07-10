{-# LANGUAGE FlexibleContexts #-}

module Errors
  ( AppError (..)
  , raiseAppError
  , resourceNotFound
  , serverError
  , badRequest
  ) where

import Control.Monad.Except
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Servant

data AppError = ResourceNotFound
              | BadRequest T.Text
              | ServerError
  deriving (Show, Eq, Read)

raiseAppError :: MonadError ServantErr m => AppError -> m a
raiseAppError ResourceNotFound    = throwError resourceNotFound
raiseAppError (BadRequest errMsg) = throwError $ badRequest errMsg
raiseAppError ServerError         = throwError serverError

resourceNotFound :: ServantErr
resourceNotFound = err404 { errBody = "The request resource could not be found" }

serverError :: ServantErr
serverError = err500 { errBody = "Internal server error" }

badRequest :: T.Text -> ServantErr
badRequest msg = err400 { errBody = encode msg }

encode :: T.Text -> BL.ByteString
encode = TLE.encodeUtf8 . TL.fromStrict
