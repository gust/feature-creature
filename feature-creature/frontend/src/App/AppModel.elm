module App.AppModel exposing
  ( App
  )

import App.AppConfig exposing (..)
import App.Products.Product exposing (Product)
import App.Products.ProductForm exposing (ProductForm)
import Data.External exposing (..)
import Hop.Types exposing (Location)
import App.Routing exposing (Route)

type alias App =
  { appConfig    : AppConfig
  , location     : Location
  , route        : Route
  , currentUser  : String
  , products     : External (List Product)
  , productForm  : ProductForm
  }
