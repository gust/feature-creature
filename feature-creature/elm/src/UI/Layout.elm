module UI.Layout exposing (withLayout)

import Html exposing (Html)
import Html.Attributes as Html

withLayout : List (Html a) -> Html a
withLayout content =
  Html.div
    [ Html.id "main_content"
    , Html.classList [ ("container-fluid", True) ]
    ]
    content
