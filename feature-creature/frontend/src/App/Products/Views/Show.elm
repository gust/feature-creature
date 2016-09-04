module App.Products.Views.Show exposing
  ( showView
  )

import App.Products.Product exposing (Product)
import Data.External exposing (..)
import Html exposing (Html)

showView : External Product -> List (Html a)
showView exProd = case exProd of
  NotLoaded             -> loadingView
  LoadedWithError error -> errorView error
  Loaded product       ->
    [ Html.div [] [ Html.text "Hey! Nice Product!" ]
    , Html.div
        []
        [ Html.text (toString product) ]
    ]

loadingView : List (Html a)
loadingView =
  [ Html.div
    []
    [ Html.text "Loading Selected Product..." ]
  ]

errorView : String -> List (Html a)
errorView error =
  [ Html.div
      []
      [ Html.text ("Oh no! " ++ error) ]
  ]
