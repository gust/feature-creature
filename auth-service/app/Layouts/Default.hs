{-# LANGUAGE QuasiQuotes #-}

module Layouts.Default
  ( withDefaultLayout
  ) where

import qualified Auth0.Config as Auth0
import Config.AppConfig
import Data.Monoid ((<>))
import Data.Text (Text)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.QuasiText

withDefaultLayout :: AppConfig -> Html -> Html
withDefaultLayout cfg content = H.docTypeHtml $ do
  header cfg
  body content

header :: AppConfig -> Html
header cfg =
  H.head $ do
    bootstrapCSS
    (authZeroJS cfg)
    meta
    H.title "This is a sample authentication service"

meta :: Html
meta =
  H.meta
    ! A.name "viewport"
    ! A.content "width=device-width, initial-scale=1"

bootstrapCSS :: Html
bootstrapCSS =
  H.link
    ! A.rel "stylesheet"
    ! A.href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"

authZeroJS :: AppConfig -> Html
authZeroJS cfg = do
  H.script ! A.src "https://cdn.auth0.com/js/lock-9.1.min.js" $ ""
  H.script $ H.text $ authZeroConfig cfg

authZeroConfig :: AppConfig -> Text
authZeroConfig cfg = do
  let (Auth0.Config auth0ApiKey _ _ _ _) = getAuthConfig cfg
  let accessCodeUrl = (getBasePath cfg) <> "/access-code"
  [embed|
    var lock = new Auth0Lock('$auth0ApiKey', 'feature-creature.auth0.com');

    function signin() {
      lock.show({
          callbackURL: '$accessCodeUrl'
        , responseType: 'code'
        , authParams: {
          scope: 'openid email'
        }
      });
    }
  |]

bootstrapJS :: Html
bootstrapJS =
  H.script
    ! A.src "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
    $ ""

body :: Html -> Html
body content =
  H.body $ do
    H.div ! A.class_ "container" $ do
      content
    jQuery
    bootstrapJS

jQuery :: Html
jQuery =
  H.script
    ! A.src "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"
    $ ""
