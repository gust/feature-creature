module App.AppModel exposing
  ( App
  )

import App.AppConfig exposing (..)
import App.Products.Product exposing (Product)
import Data.External exposing (..)
import Hop.Types exposing (Location)
import App.Routing exposing (Route)

type alias App =
  { appConfig   : AppConfig
  , products    : External (List Product)
  , location    : Location
  , route       : Route
  , currentUser : String
  }
