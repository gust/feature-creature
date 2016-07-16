module App.Products.Views.New exposing
  ( newView
  )

import Html exposing (Html)

newView : List (Html a)
newView =
  [ Html.div [] [ Html.text "This is where we're gonna make you a new product" ]
  ]
