module App.Products.Views.New exposing
  ( newView
  )

import App.Products.ProductForm exposing (ProductForm, ProductFormMsg (..), Visiblility (..))
import App.Products.Repository exposing (Repository)
import Data.External exposing (..)
import Html exposing (Html)
import Html as H
import Html.Attributes as A
import Html.Events as E
import UI.Bootstrap.Modal as BS

newView : ProductForm -> List (Html ProductFormMsg)
newView pForm = case pForm.repositories of
  NotLoaded -> loadingView
  LoadedWithError error -> errorView error
  Loaded [] -> blankState
  Loaded rs ->
    [ H.div [] <| List.concat [repositorySelectList rs, confirmationModal pForm] ]

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

confirmationModal : ProductForm -> List (Html ProductFormMsg)
confirmationModal pForm = case pForm.selectedRepository of
  Nothing -> [ Html.div [] [] ]
  (Just repository) -> case pForm.modalVisibility of
    Hidden -> [ Html.div [] [] ]
    Visible ->
      let mTitle = H.text "hello"
          mBody = H.div [] [ H.div [] [ H.text "Hi from the modal body!" ] ]
          mFooter = H.div [] [ H.div [] [ H.text "Hi from the modal footer!" ] ]
      in [ BS.modal mTitle mBody mFooter CloseConfirmationModal ]

blankState : List (Html a)
blankState =
  [ H.div [] [ H.text "It doesn't look like you have any repositories available" ] ]

loadingView : List (Html a)
loadingView =
  [ H.div [] [ H.text "Loading Repositories..." ] ]

errorView : String -> List (Html a)
errorView error =
  [ H.div [] [ H.text ("Oh no! " ++ error) ] ]
