module Home.View
  ( renderShowA
  ) where

import Layouts.Default (withDefaultLayout)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

renderShowA :: Html
renderShowA = withDefaultLayout $ do
  H.p "Hello! I'm Feature Creature's auth site!"
  H.p $ do
    H.button
      ! A.onclick "window.signin();"
      $ "Login"
