module App.Products.Views.Index exposing
  ( indexView
  , loadingView
  )

import App.Products.Product exposing (Product)
import Data.External exposing (..)
import Html exposing (Html)

indexView : External (List Product) -> List (Html a)
indexView exProd = case exProd of
  NotLoaded             -> loadingView
  LoadedWithError error -> errorView error
  Loaded products       ->
    [ Html.div
        []
        [ Html.text ("You have " ++ (toString <| List.length products) ++ " products!") ]
    , Html.ul
        []
        (List.map (\product -> Html.li [] [ Html.text (toString product) ]) products)
    ]

loadingView : List (Html a)
loadingView =
  [ Html.div
    []
    [ Html.text "Loading Products..." ]
  ]

errorView : String -> List (Html a)
errorView error =
  [ Html.div
      []
      [ Html.text ("Oh no! " ++ error) ]
  ]
