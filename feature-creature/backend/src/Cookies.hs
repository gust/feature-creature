module Cookies
  ( parseCookies
  ) where

import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

type Cookies = Map Text Text

parseCookies :: ByteString -> Cookies
parseCookies cookies =
  Map.fromList $ mapMaybe kvList cs
  where
    cs :: [Text] 
    cs = T.split (== ';') $ TE.decodeUtf8 cookies

    kvList :: Text -> Maybe (Text, Text)
    kvList c = case T.split (== '=') c of
      (k:v:[]) -> Just (T.strip k, T.strip v)
      _     -> Nothing
