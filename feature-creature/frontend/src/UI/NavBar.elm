module UI.NavBar exposing
  ( NavBarItem
  , OrientedNavItem (..)
  , renderNavBar
  )
import Html exposing (Html, Attribute)
import Html.Attributes exposing (class, classList, attribute, href, id)
import Html.Attributes as Html

type alias NavBarItem a =
  { attributes : List (Attribute a)
  , html       : List (Html a)
  }

type OrientedNavItem a = LeftNav (List (NavBarItem a))
                       | RightNav (List (NavBarItem a))

renderNavBar : OrientedNavItem a
            -> OrientedNavItem a
            -> Html a
renderNavBar navBarLeftItems navBarRightItems =
  Html.nav
    [ classList [("navbar", True), ("navbar-inverse", True)] ]
    [ Html.div
        [ class "container-fluid" ]
        [ navBarHeader
        , renderItems navBarLeftItems navBarRightItems
        ]
    ]

renderItems : OrientedNavItem a
           -> OrientedNavItem a
           -> Html a
renderItems leftItems rightItems =
  Html.div
    [ classList [("collapse", True), ("navbar-collapse", True)]
    , id "main_nav"
    ]
    [ renderNavItems leftItems
    , renderNavItems rightItems
    ]

renderNavItems : OrientedNavItem a -> Html a
renderNavItems items =
  let defaultClasses = [("nav", True), ("navbar-nav", True)]
  in
    case items of
      LeftNav leftItems ->
        Html.ul
          [ classList (defaultClasses ++ [("navbar-left", True)]) ]
          (List.map renderItem leftItems)
      RightNav rightItems ->
        Html.ul
          [ classList (defaultClasses ++ [("navbar-right", True)]) ]
          (List.map renderItem rightItems)

renderItem : NavBarItem a -> Html a
renderItem item =
  Html.li item.attributes item.html

navBarHeader : Html a
navBarHeader =
  Html.div
    [ class "navbar-header" ]
    [ navBarCollapsibleNavButton
    , navBarBrandButton
    ]

navBarBrandButton : Html a
navBarBrandButton =
  Html.a
    [ class "navbar-brand", href "#" ]
    [ Html.text "FeatureCreature" ]

navBarCollapsibleNavButton : Html a
navBarCollapsibleNavButton =
  Html.button
    [ attribute "type" "button"
    , attribute "data-toggle" "collapse"
    , attribute "data-target" "#main_nav"
    , attribute "aria-expanded" "false"
    , classList [("navbar-toggle", True), ("collapsed", True)]
    ]
    [ Html.span [ class "icon-bar" ] []
    , Html.span [ class "icon-bar" ] []
    , Html.span [ class "icon-bar" ] []
    ]
