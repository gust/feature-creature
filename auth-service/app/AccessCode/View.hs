module AccessCode.View
  ( renderShowA
  , renderError
  ) where

import Auth0.User (User)
import Data.Text (Text)
import Layouts.Default (withDefaultLayout)
import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as H

renderShowA :: User -> Html
renderShowA user = withDefaultLayout $ do
  renderUser user

renderUser :: User -> Html
renderUser user = H.string $ show user

renderError :: Text -> Html
renderError err = withDefaultLayout $ do
  H.p
    "There was a problem."
  H.p $
    H.text err
