module AccessTokens
  ( withAccessToken
  ) where

import App (AppT)
import Cookies (parseCookies)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Map as Map
import Errors (raiseMissingAccessTokenError)

withAccessToken :: Text -> (Text -> AppT a) -> AppT a
withAccessToken cookies f =
  case Map.lookup "access-token" (parseCookies $ TE.encodeUtf8 cookies) of
    Nothing    -> raiseMissingAccessTokenError
    Just token -> f token
