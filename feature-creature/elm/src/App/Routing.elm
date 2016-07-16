module App.Routing exposing
  ( Route (..)
  , RouteMsg (..)
  , redirectTo
  , routerConfig
  , parseRoute
  , urlParser
  )

import Navigation
import Hop exposing (makeUrl, makeUrlFromLocation, matcherToPath, matchUrl, setQuery)
import Hop.Types exposing (Config, Query, Location, PathMatcher, Router)
import Hop.Matchers exposing (..)
import Task

type Route = HomeRoute
           | ProductsRoute
           | NewProductRoute
           | NotFoundRoute

type RouteMsg = NavigateTo String
              | SetQuery Query

routerConfig : Config Route
routerConfig =
  { hash = False
  , basePath = ""
  , matchers = matchers
  , notFound = NotFoundRoute
  }

urlParser : Navigation.Parser (Route, Hop.Types.Location)
urlParser = Navigation.makeParser (.href >> matchUrl routerConfig)

redirectTo : Route -> Cmd RouteMsg
redirectTo route =
  let id = \a -> a
      msg = NavigateTo <| parseRoute route
  in Task.perform id id <| Task.succeed msg

parseRoute : Route -> String
parseRoute route =
  case route of
    HomeRoute       -> matcherToPath homeMatcher []
    ProductsRoute   -> matcherToPath productsMatcher []
    NewProductRoute -> matcherToPath newProductMatcher []
    NotFoundRoute   -> ""

matchers : List (PathMatcher Route)
matchers =
  [ homeMatcher
  , productsMatcher
  , newProductMatcher
  ]

homeMatcher : PathMatcher Route
homeMatcher = match1 HomeRoute ""

productsMatcher : PathMatcher Route
productsMatcher = match1 ProductsRoute "/products"

newProductMatcher : PathMatcher Route
newProductMatcher = match1 NewProductRoute "/products/new"
