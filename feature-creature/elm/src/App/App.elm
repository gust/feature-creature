module App.App exposing
  ( App
  , init
  , update
  , view
  )

import App.AppConfig exposing (..)
import App.Products.Product as P
import App.Products.Requests exposing (getProducts)
import Data.External exposing (..)
import Html exposing (Html)
import Html.Attributes as Html

type alias App =
  { appConfig : AppConfig
  , products  : External (List P.Product)
  }

type AppMsg = ProductMsg P.ProductMsg

init : AppConfig -> (App, Cmd AppMsg)
init appConfig =
  let initialState = { appConfig = appConfig
                     , products  = NotLoaded
                     }
  in (initialState, Cmd.map ProductMsg (getProducts appConfig))

update : a -> App -> (App, Cmd a)
update msg app = (app, Cmd.none)

view : App -> Html a
view app =
  Html.div
    [ Html.id "main_content"
    , Html.classList [ ("container-fluid", True) ]
    ]
    [
      Html.div [] [ Html.text ("Hi there " ++ (app.appConfig.user)) ]
    ]
