{-# LANGUAGE FlexibleContexts #-}

module Errors
  ( AppError (..)
  , authenticationRequired
  , raiseAppError
  , raiseMissingAccessTokenError
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
              | AuthenticationRequired
              | ServerError (Maybe T.Text)
  deriving (Show, Eq, Read)

raiseAppError :: MonadError ServantErr m => AppError -> m a
raiseAppError ResourceNotFound       = throwError resourceNotFound
raiseAppError (BadRequest errMsg)    = throwError $ badRequest errMsg
raiseAppError AuthenticationRequired = throwError authenticationRequired
raiseAppError (ServerError errMsg)   = throwError $ serverError errMsg

raiseMissingAccessTokenError :: MonadError ServantErr m => m a
raiseMissingAccessTokenError = raiseAppError (BadRequest "Missing access-token cookie")

authenticationRequired :: ServantErr
authenticationRequired = err401 { errBody = "Authentication required" }

resourceNotFound :: ServantErr
resourceNotFound = err404 { errBody = "The request resource could not be found" }

serverError :: Maybe T.Text -> ServantErr
serverError Nothing = err500 { errBody = "Internal server error" }
serverError (Just err) = err500 { errBody = encode err }

badRequest :: T.Text -> ServantErr
badRequest msg = err400 { errBody = encode msg }

encode :: T.Text -> BL.ByteString
encode = TLE.encodeUtf8 . TL.fromStrict
