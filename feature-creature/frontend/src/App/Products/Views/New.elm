module App.Products.Views.New exposing
  ( newView
  )

import App.Products.ProductForm exposing (ProductForm, ProductFormMsg (..))
import App.Products.Repository exposing (Repository)
import Data.External exposing (..)
import Html exposing (Html)
import Html as H
import Html.Attributes as A
import Html.Events as E

newView : ProductForm -> Html ProductFormMsg
newView pForm = case pForm.repositories of
  NotLoaded -> loadingView
  LoadedWithError error -> errorView error
  Loaded [] -> blankState
  Loaded rs -> H.div [] (repositorySelectList rs)

repositorySelectList : List Repository -> List (Html ProductFormMsg)
repositorySelectList repositories =
  [ H.div [] [ H.text "Here are all of your repositories:" ]
  , H.div [] [ repositoryList repositories ]
  ]

repositoryList : List Repository -> Html ProductFormMsg
repositoryList repositories =
  H.div
    [ A.classList [ ("list-group", True) ] ]
    (List.map repositoryListItem repositories)

repositoryListItem : Repository -> Html ProductFormMsg
repositoryListItem repository =
  let owner = repository.owner
  in H.button
    [ A.type' "button"
    , A.classList [ ("list-group-item", True) ]
    , E.onClick (RepositorySelected repository)
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

blankState : Html a
blankState =
  H.div [] [ H.text "It doesn't look like you have any repositories available" ]

loadingView : Html a
loadingView =
  H.div [] [ H.text "Loading Repositories..." ]

errorView : String -> Html a
errorView error =
  H.div [] [ H.text ("Oh no! " ++ error) ]
