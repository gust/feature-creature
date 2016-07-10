module Home.View
  ( showA
  ) where

import Data.Monoid ((<>))
import Data.Text
import Layouts.Default (withDefaultLayout)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Users.Api

showA :: Maybe User -> String -> Html
showA Nothing authUrl = withDefaultLayout $ renderLoggedOutView authUrl
showA (Just user) _ = withDefaultLayout $ renderLoggedInView user

renderLoggedInView :: User -> Html
renderLoggedInView user = do
  H.p $ do
    H.text $ "Hello from Feature Creature! " <> (name user)
  H.p $ do
    H.text $ "Here's all the info we have on you:"
  H.p $
    H.text $ "ID - " <> (pack . show $ (Users.Api.id user))
  H.p $
    H.text $ "Auth Provider ID - " <> authProviderId user
  H.p $
    H.text $ "Email - " <> email user
  H.p $
    H.text $ "Name - " <> name user
  H.p $ do
    _ <- "Let's head over to the "
    H.a ! A.href (H.toValue ("/private"::String)) $ "private content"

renderLoggedOutView :: String -> Html
renderLoggedOutView authUrl = do
  H.p "Hello! I'm Feature Creature!"
  H.p "You are not logged in."
  H.p $ do
    _ <- "Let's head over to the "
    H.a ! A.href (H.toValue authUrl) $ "sign in page"
