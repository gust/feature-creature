module Retry
( withRetry
, withRetryStatus
) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Control.Retry as CR
import Data.Monoid ((<>))

withRetry :: (MonadIO m, MonadMask m) => m a -> m a
withRetry f = CR.recoverAll defaultRetryPolicy (const f)

withRetryStatus :: (MonadIO m, MonadMask m) => m a -> m a
withRetryStatus f = CR.recoverAll defaultRetryPolicy (\rs -> liftIO (print (show rs)) >> f)

defaultRetryPolicy :: (MonadIO m, MonadMask m) => CR.RetryPolicyM m
defaultRetryPolicy = everySecondForTwoMinutes

everySecondForTwoMinutes :: (MonadIO m, MonadMask m) => CR.RetryPolicyM m
everySecondForTwoMinutes = everySecond <> forMinutes 1

everySecond :: (MonadIO m, MonadMask m) => CR.RetryPolicyM m
everySecond = CR.constantDelay 1000000

forMinutes :: (MonadIO m, MonadMask m) => Int -> CR.RetryPolicyM m
forMinutes m = CR.limitRetries (m * 60)

