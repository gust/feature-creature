module Private.View
  ( showA
  ) where

import Data.Monoid ((<>))
import Layouts.Default (withDefaultLayout)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Users.Api (User (..))

showA :: User -> Html
showA user = withDefaultLayout $ do
  H.p $ do
    H.text $ "Hi " <> (name user)
  H.p "I'm a private page!"
  H.p "Here's the secret combination to my luggage: 12345"
  H.p $ do
    _ <- "Let's back to the "
    H.a ! A.href (H.toValue ("/"::String)) $ "home page"

