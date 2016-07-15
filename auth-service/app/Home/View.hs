module Home.View
  ( renderShowA
  ) where

import App (AppT)
import Control.Monad.Reader
import Layouts.Default (withDefaultLayout)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

renderShowA :: AppT Html
renderShowA = ask >>= \cfg -> return $ withDefaultLayout cfg $ do
  H.p "Hello! I'm Feature Creature's auth site!"
  H.p $ do
    H.button
      ! A.onclick "window.signin();"
      $ "Login"
