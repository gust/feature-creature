module App.Products.Views.New exposing
  ( newView
  )

import App.Products.Repository exposing (Repository)
import Data.External exposing (..)
import Html exposing (Html)

newView : External (List Repository) -> List (Html a)
newView exRepo = case exRepo of
  NotLoaded             -> loadingView
  LoadedWithError error -> errorView error
  Loaded repositories   ->
    if List.length repositories == 0 then
      blankState
    else
      [ Html.div [] [ Html.text "Here are all of your repositories:" ]
      , Html.ul
          []
          (List.map (\repository -> Html.li [] [ Html.text (toString repository) ]) repositories)
      ]

blankState : List (Html a)
blankState =
  [ Html.div [] [ Html.text "It doesn't look like you have any repositories available" ] ]

loadingView : List (Html a)
loadingView =
  [ Html.div
    []
    [ Html.text "Loading Repositories..." ]
  ]

errorView : String -> List (Html a)
errorView error =
  [ Html.div
      []
      [ Html.text ("Oh no! " ++ error) ]
  ]
