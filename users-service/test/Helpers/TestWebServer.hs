module Helpers.TestWebServer
  ( module Helpers.RequestSpecHelpers
  , app
  , testAppConfig
  , with
  , withWebServer
  ) where

import App (AppT, AppConfig (..), getAppConfig, runAppT)
import Config.Environment (Environment (Test))
import Helpers.DatabaseHelpers
import Helpers.RequestSpecHelpers
import LoadEnv
import Network.Wai (Application)
import Routing (API, server, api)
import Servant
import Test.Hspec.Wai (with)

withWebServer :: SpecWith Application -> Spec
withWebServer = with (testAppConfig >>= app)

testAppConfig :: IO AppConfig
testAppConfig =
  loadEnvFrom "./env/test.env" >> getAppConfig "dummy" Test

app :: AppConfig -> IO Application
app cfg = do
  setupTestDatabase $ getDBConn cfg
  return $ serve api (readerServer cfg)

readerServer :: AppConfig -> Server API
readerServer cfg = enter (readerToEither cfg) server

readerToEither :: AppConfig -> AppT :~> Handler
readerToEither cfg = Nat $ \appT -> runAppT cfg appT
