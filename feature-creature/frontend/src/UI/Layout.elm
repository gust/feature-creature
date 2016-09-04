module UI.Layout exposing
  ( withLayout
  , mainNavigation
  )

import App.AppModel exposing (App)
import App.Products.Product exposing (Product)
import App.Routing as R
import Data.External exposing (..)
import Html exposing (Html, Attribute)
import Html.Attributes exposing (class, classList, attribute, href, id)
import Html.Attributes as Html
import Html.Events exposing (onClick)
import UI.NavBar as Nav

withLayout : App -> List (Html a) -> Html a
withLayout app content =
  Html.div
    [ Html.id "main_content"
    , Html.classList [ ("container-fluid", True) ]
    ]
    content

mainNavigation : App -> Html R.RouteMsg
mainNavigation app = case app.products of
  (Loaded products) ->
    Nav.renderNavBar
      (Nav.LeftNav [ productsDropdownItem products ])
      (Nav.RightNav [ userItem app.currentUser ])
  _ ->
    Html.div [] []

userItem : String -> Nav.NavBarItem a
userItem currentUser =
  { attributes = [ classList [("active", False)] ]
  , html = [ Html.a [] [ Html.text <| "Welcome " ++ currentUser ] ]
  }

productsDropdownItem : List Product -> Nav.NavBarItem R.RouteMsg
productsDropdownItem products =
  { attributes = [ class "dropdown" ]
  , html =
    [ Html.a
        [ class "dropdown-toggle"
        , attribute "data-toggle" "dropdown"
        , attribute "role" "button"
        ]
        [ Html.text "Products"
        , Html.span [ class "caret" ] []
        ]
    , Html.ul [ class "dropdown-menu" ]
        <| List.map (renderProductItem) products ++
            [ Html.li [ class "divider", attribute "role" "separator" ] []
            , Html.li [ onClick (R.NavigateTo <| R.parseRoute R.NewProductRoute) ] [ Html.a [] [ Html.text "Create New Product" ] ]
            ]
    ]
  }

renderProductItem : Product -> Html a
renderProductItem product =
  Html.li [] [ Html.a [] [ Html.text product.name ] ]
