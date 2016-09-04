module Main exposing (..)

import App.App  as App
import App.AppConfig exposing (..)
import App.Routing exposing (urlParser)
import Navigation

main : Program InitialConfig
main = Navigation.programWithFlags urlParser
  { init      = App.init
  , update    = App.update
  , urlUpdate = App.urlUpdate
  , view      = App.view
  , subscriptions = \_ -> Sub.none
  }
