module UI.Layout exposing (withLayout)

import App.AppModel exposing (App)
import Html exposing (Html, Attribute)
import Html.Attributes exposing (class, classList, attribute, href, id)
import Html.Attributes as Html
import UI.NavBar as Nav

withLayout : App -> List (Html a) -> Html a
withLayout app content =
  Html.div
    [ Html.id "main_content"
    , Html.classList [ ("container-fluid", True) ]
    ]
    (mainNavigation app :: content)

mainNavigation : App -> Html a
mainNavigation app =
  Nav.renderNavBar
    (Nav.LeftNav [])
    (Nav.RightNav [ userItem app.currentUser ])

userItem : String -> Nav.NavBarItem a
userItem currentUser =
  { attributes = [ classList [("active", False)] ]
  , html = [ Html.a [] [ Html.text <| "Welcome " ++ currentUser ] ]
  }
