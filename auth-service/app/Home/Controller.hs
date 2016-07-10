{-# LANGUAGE FlexibleContexts #-}

module Home.Controller
  ( showA
  ) where

import App (AppT)
import qualified Home.View as V
import Text.Blaze.Html5 (Html)

showA :: AppT Html
showA = return V.renderShowA
