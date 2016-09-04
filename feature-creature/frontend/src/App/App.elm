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
import App.Products.Requests as P
import App.Products.Views.Index as P
import App.Products.Views.Show as P
import App.Products.Views.New as P
import App.Routing exposing (Route (..), RouteMsg (..), redirectTo, routerConfig)
import App.Views.FourOhFour exposing (fourOhFour)
import Data.External exposing (..)
import Hop exposing (makeUrl, makeUrlFromLocation, setQuery)
import Hop.Types exposing (Location)
import Html exposing (Html)
import Html.App as Html
import Navigation
import UI.Layout as UI

type AppMsg = NavigationMsg RouteMsg
            | ProductMsg P.ProductMsg
            | ProductFormMsg PF.ProductFormMsg

init : InitialConfig -> (Route, Location) -> (App, Cmd AppMsg)
init initialConfig (route, location) =
  let appConfig = toAppConfig initialConfig
      state = { appConfig       = appConfig
              , route           = route
              , location        = location
              , currentUser     = appConfig.user
              , products        = NotLoaded
              , selectedProduct = NotLoaded
              , productForm     = PF.mkProductForm
              }
      cmd = Cmd.map ProductMsg (P.getProducts appConfig)
  in (state, cmd)

update : AppMsg -> App -> (App, Cmd AppMsg)
update msg app =
  let appMsg = logMsg' app.appConfig "(App.update) msg: " msg
  in case appMsg of
      ProductMsg msg     -> handleProductMessage msg app
      ProductFormMsg msg -> handleProductFormMessage msg app
      NavigationMsg msg  -> handleNavigationMessage msg app

handleProductMessage : P.ProductMsg -> App -> (App, Cmd AppMsg)
handleProductMessage msg app = case msg of
  (P.FetchProductsSucceeded []) ->
    ( { app | products = Loaded [] }
    , Cmd.map NavigationMsg <| redirectTo NewProductRoute
    )

  (P.FetchProductsSucceeded products) ->
    let cmd = Cmd.map NavigationMsg (redirectTo ProductsRoute)
    in ({ app | products = Loaded products }, cmd)

  (P.FetchProductsFailed err) ->
    let products = LoadedWithError "Failed to load products!"
        cmd      = Cmd.map NavigationMsg (redirectTo ProductsRoute)
        result   = ({ app | products = products }, cmd)
    in logMsg' app.appConfig ("Error: " ++ toString err) result

  -- We need to account for this pattern, but we should never receive
  -- this event. How can we do this better?
  (P.CreateProductSucceeded product) ->
    logMsg' app.appConfig ("Unexpected event source!") (app, Cmd.none)

  -- We need to account for this pattern, but we should never receive
  -- this event. How can we do this better?
  (P.CreateProductFailed err) ->
    logMsg' app.appConfig ("Unexpected event source!") (app, Cmd.none)

handleProductFormMessage : PF.ProductFormMsg -> App -> (App, Cmd AppMsg)
handleProductFormMessage msg app = case msg of
  (PF.ProductMsg (P.CreateProductSucceeded product)) ->
    (app, Cmd.map ProductMsg (P.getProducts app.appConfig))

  (PF.ProductMsg (P.CreateProductFailed err)) ->
    let product = LoadedWithError "Failed to create product!"
        cmd      = Cmd.map NavigationMsg (redirectTo ProductsRoute)
        result   = ({ app | selectedProduct = product }, cmd)
    in logMsg' app.appConfig ("Error: " ++ toString err) result

  _ ->
    let (pForm, pFormMsg) = PF.update msg app.productForm app.appConfig
    in ({ app | productForm = pForm}, Cmd.map ProductFormMsg pFormMsg)

handleNavigationMessage : RouteMsg -> App -> (App, Cmd AppMsg)
handleNavigationMessage msg app = case msg of
  (NavigateTo path) -> case path of
    "/products/new" ->
      let (pForm, pFormCmd)    = PF.update PF.InitializeForm app.productForm app.appConfig
          (app, navigationCmd) = navigateTo app (NavigateTo path)
          cmds                 = [navigationCmd, Cmd.map ProductFormMsg pFormCmd]
      in { app | productForm = pForm } ! cmds
    _ ->
      navigateTo app (NavigateTo path)

  _ -> navigateTo app msg

view : App -> Html AppMsg
view app = case app.route of
  HomeRoute       -> UI.withLayout app (mainNavigation app :: P.loadingView)
  ProductsRoute   -> UI.withLayout app (mainNavigation app :: P.indexView app.products)
  ProductRoute id -> UI.withLayout app (mainNavigation app :: P.showView app.selectedProduct)
  NewProductRoute -> UI.withLayout app (mainNavigation app :: (Html.map ProductFormMsg <| P.newView app.productForm) :: [])
  NotFoundRoute   -> UI.withLayout app (mainNavigation app :: fourOhFour)

mainNavigation : App -> Html AppMsg
mainNavigation app = Html.map NavigationMsg <| UI.mainNavigation app

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
