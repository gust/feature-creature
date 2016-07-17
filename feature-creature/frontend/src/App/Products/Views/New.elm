module App.Products.Views.New exposing
  ( newView
  )

import App.Products.Repository exposing (Repository)
import Data.External exposing (..)
import Html exposing (Html)
import Html.Attributes as Attrs

newView : External (List Repository) -> List (Html a)
newView exRepo = case exRepo of
  NotLoaded             -> loadingView
  LoadedWithError error -> errorView error
  Loaded repositories   ->
    if List.length repositories == 0 then
      blankState
    else
      repositorySelectList repositories

repositorySelectList : List Repository -> List (Html a)
repositorySelectList repositories =
  [ Html.div [] [ Html.text "Here are all of your repositories:" ]
  , Html.form
      []
      [ repositoryList repositories
      ]
  ]

repositoryList : List Repository -> Html a
repositoryList repositories =
  Html.div
    [ Attrs.classList [ ("list-group", True) ] ]
    (List.map repositoryListItem repositories)

repositoryListItem : Repository -> Html a
repositoryListItem repository =
  let owner = repository.owner
  in Html.button
    [ Attrs.type' "button"
    , Attrs.classList [ ("list-group-item", True) ]
    ]
    [ Html.text repository.name
    , Html.div
        [ Attrs.classList [ ("pull-left", True) ]
        , Attrs.style [ ("margin-right", "10px") ]
        ]
        [ Html.img
            [ (Attrs.src (owner.avatarUrl))
            , (Attrs.height 25)
            , (Attrs.width 25)
            , (Attrs.class "img-circle")
            ]
            []
        ]
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
