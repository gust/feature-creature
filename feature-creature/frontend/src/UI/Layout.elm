module UI.Layout exposing (withLayout)

import App.AppModel exposing (App)
import Html exposing (Html)
import Html.Attributes as Html

withLayout : App -> List (Html a) -> Html a
withLayout app content =
  Html.div
    [ Html.id "main_content"
    , Html.classList [ ("container-fluid", True) ]
    ]
    (mainNavigation app :: content)

mainNavigation : App -> Html a
mainNavigation app =
  Html.div
    []
    [ Html.text <| "Welcome " ++ (toString app.currentUser) ]
