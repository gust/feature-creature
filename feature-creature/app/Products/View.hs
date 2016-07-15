{-# LANGUAGE QuasiQuotes #-}

module Products.View
  ( showA
  ) where

import Layouts.Default (withDefaultLayout)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.QuasiText
import Users.Api (User (..))

showA :: User -> Html
showA user = withDefaultLayout $ do
  let userName = name user
  H.div ! A.id "app" $ H.script $ H.text $
    [embed|
      init('$userName');
    |]
