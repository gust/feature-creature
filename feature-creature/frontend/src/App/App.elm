module App.App exposing
  ( init
  , update
  , view
  , urlUpdate
  )

import App.AppConfig exposing (..)
import App.AppModel exposing (App)
import App.Products.Product as P
import App.Products.ProductForm as PF
import App.Products.ProductFormView as PF
import App.Products.Requests as P
import App.Products.Views.Index as P
import App.Routing exposing (Route (..), RouteMsg (..), redirectTo, routerConfig)
import Data.External exposing (..)
import Hop exposing (makeUrl, makeUrlFromLocation, setQuery)
import Hop.Types exposing (Location)
import Html exposing (Html)
import Navigation
import UI.Layout as UI

type AppMsg = NavigationMsg RouteMsg
            | ProductMsg P.ProductMsg
            | ProductFormMsg PF.ProductFormMsg

init : InitialConfig -> (Route, Location) -> (App, Cmd AppMsg)
init initialConfig (route, location) =
  let appConfig = toAppConfig initialConfig
      state = { appConfig    = appConfig
              , products     = NotLoaded
              , route        = route
              , location     = location
              , currentUser  = appConfig.user
              , productForm  = PF.mkProductForm
              }
      cmd = Cmd.map ProductMsg (P.getProducts appConfig)
  in (state, cmd)

update : AppMsg -> App -> (App, Cmd AppMsg)
update msg app =
  let appMsg = logMsg' app.appConfig "(App.update) msg: " msg
  in case appMsg of
      ProductMsg (P.FetchProductsSucceeded []) ->
        let (pForm, pFormCmd) = PF.update PF.InitializeForm app.productForm app.appConfig
            navigationCmd     = Cmd.map NavigationMsg <| redirectTo NewProductRoute
            cmds              = [navigationCmd, Cmd.map ProductFormMsg pFormCmd]
        in { app | products = Loaded []
                 , productForm = pForm
           } ! cmds

      ProductMsg (P.FetchProductsSucceeded products) ->
        let cmd = Cmd.map NavigationMsg (redirectTo ProductsRoute)
        in ({ app | products = Loaded products }, cmd)

      ProductMsg (P.FetchProductsFailed err) ->
        let products = LoadedWithError "Failed to load products!"
            cmd      = Cmd.map NavigationMsg (redirectTo ProductsRoute)
            result   = ({ app | products = products }, cmd)
        in logMsg' app.appConfig ("Error: " ++ toString err) result

      ProductFormMsg msg ->
        let (pForm, pFormMsg) = PF.update msg app.productForm app.appConfig
        in ({ app | productForm = pForm}, Cmd.map ProductFormMsg pFormMsg)

      NavigationMsg msg -> navigateTo app msg

view : App -> Html a
view app = case app.route of
  HomeRoute       -> UI.withLayout app P.loadingView
  ProductsRoute   -> UI.withLayout app <| P.indexView app.products
  NewProductRoute -> UI.withLayout app <| PF.view app.productForm
  NotFoundRoute   -> UI.withLayout app fourOhFour

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
