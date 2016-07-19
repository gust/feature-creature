module App.Products.ProductFormView exposing
  ( view
  )

import App.Products.ProductForm exposing (ProductForm)
import App.Products.Repository exposing (Repository)
import Data.External exposing (..)
import Html exposing (Html)
import Html as H
import Html.Attributes as A

view : ProductForm -> List (Html a)
view pForm = case pForm.repositories of
  NotLoaded             -> loadingView
  LoadedWithError error -> errorView error
  Loaded rs   ->
    if List.length rs == 0 then
      blankState
    else
      repositorySelectList rs

repositorySelectList : List Repository -> List (Html a)
repositorySelectList repositories =
  [ H.div [] [ H.text "Here are all of your repositories:" ]
  , H.form [] [ repositoryList repositories ]
  ]

repositoryList : List Repository -> Html a
repositoryList repositories =
  H.div
    [ A.classList [ ("list-group", True) ] ]
    (List.map repositoryListItem repositories)

repositoryListItem : Repository -> Html a
repositoryListItem repository =
  let owner = repository.owner
  in H.button
    [ A.type' "button"
    , A.classList [ ("list-group-item", True) ]
    ]
    [ H.text repository.name
    , H.div
        [ A.classList [ ("pull-left", True) ]
        , A.style [ ("margin-right", "10px") ]
        ]
        [ userAvatar owner.avatarUrl ]
    ]

userAvatar : String -> Html a
userAvatar avatarUrl =
  H.img
    [ (A.src (avatarUrl))
    , (A.height 25)
    , (A.width 25)
    , (A.class "img-circle")
    ]
    []

blankState : List (Html a)
blankState =
  [ H.div [] [ H.text "It doesn't look like you have any repositories available" ] ]

loadingView : List (Html a)
loadingView =
  [ H.div [] [ H.text "Loading Repositories..." ] ]

errorView : String -> List (Html a)
errorView error =
  [ H.div [] [ H.text ("Oh no! " ++ error) ] ]
