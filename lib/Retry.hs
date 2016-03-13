module Retry
( withRetry
) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Control.Retry as CR
import Data.Monoid ((<>))

withRetry :: (MonadIO m, MonadMask m) => m a -> m a
withRetry f = CR.recoverAll defaultRetryPolicy (\_ -> f)

defaultRetryPolicy :: (MonadIO m, MonadMask m) => CR.RetryPolicyM m
defaultRetryPolicy = everySecondForTwoMinutes

everySecondForTwoMinutes :: (MonadIO m, MonadMask m) => CR.RetryPolicyM m
everySecondForTwoMinutes = everySecond <> (forMinutes 2)

everySecond :: (MonadIO m, MonadMask m) => CR.RetryPolicyM m
everySecond = CR.constantDelay 1000000

forMinutes :: (MonadIO m, MonadMask m) => Int -> CR.RetryPolicyM m
forMinutes m = CR.limitRetries (m * 60)
