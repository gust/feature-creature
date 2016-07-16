module App.App exposing
  ( init
  , update
  , view
  , urlUpdate
  )

import App.AppConfig exposing (..)
import App.AppModel exposing (App)
import App.Products.Product as P
import App.Products.Requests as P
import App.Products.Views.Index as P
import App.Products.Views.New as P
import App.Routing exposing (Route (..), RouteMsg (..), redirectTo, routerConfig)
import Data.External exposing (..)
import Hop exposing (makeUrl, makeUrlFromLocation, setQuery)
import Hop.Types exposing (Location)
import Html exposing (Html)
import Http exposing (Error)
import Navigation
import UI.Layout as UI

type AppMsg = NavigationMsg RouteMsg
            | ProductMsg P.ProductMsg

init : InitialConfig -> (Route, Location) -> (App, Cmd AppMsg)
init initialConfig (route, location) =
  let appConfig = toAppConfig initialConfig
      state = { appConfig   = appConfig
              , products    = NotLoaded
              , route       = route
              , location    = location
              , currentUser = appConfig.user
              }
      cmd = Cmd.map ProductMsg (P.getProducts appConfig)
  in (state, cmd)

update : AppMsg -> App -> (App, Cmd AppMsg)
update msg app =
  let appMsg = logMsg' app.appConfig "(App.update) msg: " msg
  in case appMsg of
      ProductMsg (P.FetchProductsSucceeded products) -> handleProductsLoaded app products
      ProductMsg (P.FetchProductsFailed err) -> handleProductsLoadedFailure app err
      NavigationMsg msg -> navigateTo app msg

view : App -> Html a
view app = case app.route of
  HomeRoute       -> UI.withLayout app P.loadingView
  ProductsRoute   -> UI.withLayout app <| P.indexView app.products
  NewProductRoute -> UI.withLayout app P.newView
  NotFoundRoute   -> UI.withLayout app fourOhFour

handleProductsLoaded : App -> List P.Product -> (App, Cmd AppMsg)
handleProductsLoaded app products =
  let cmd = if List.length products == 0 then
              Cmd.map NavigationMsg (redirectTo NewProductRoute)
            else
              Cmd.none
  in ({ app | products = Loaded products }, cmd)

handleProductsLoadedFailure : App -> Error -> (App, Cmd AppMsg)
handleProductsLoadedFailure app err =
  let products = LoadedWithError "Failed to load products!"
      result = ({ app | products = products }, Cmd.none)
  in logMsg' app.appConfig ("Error: " ++ toString err) result

navigateTo : App -> RouteMsg -> (App, Cmd AppMsg)
navigateTo app msg = case msg of
  NavigateTo path ->
    let cmd = makeUrl routerConfig path |> Navigation.newUrl
    in (app, cmd)
  SetQuery query ->
    let cmd = app.location
                |> setQuery query
                |> makeUrlFromLocation routerConfig
                |> Navigation.newUrl
    in (app, cmd)

urlUpdate : (Route, Hop.Types.Location) -> App -> (App, Cmd AppMsg)
urlUpdate (route, location) app =
  ({ app | route = route, location = location }, Cmd.none)

fourOhFour : List (Html a)
fourOhFour =
  [ Html.div
      []
      [ Html.text "There's nothing here. How'd you get here, anyway?" ]
  ]
