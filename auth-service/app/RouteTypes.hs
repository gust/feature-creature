module RouteTypes
  ( WithAuthCookie
  ) where

import Data.ByteString.Lazy (ByteString)
import Servant

-- TODO: create a ToByteString instance of an AccessToken
type WithAuthCookie a = Headers '[Header "Set-Cookie" ByteString] a

