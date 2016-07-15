module App.App exposing
  ( App
  , init
  , update
  , view
  )

import App.AppConfig exposing (..)
import Html exposing (Html)
import Html.Attributes as Html

type alias App =
  { appConfig : AppConfig
  }

init : AppConfig -> (App, Cmd a)
init appConfig =
  let initialState = { appConfig   = appConfig
                     }
  in
    (initialState, Cmd.none)

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
