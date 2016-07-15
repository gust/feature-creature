module App.App exposing
  ( App
  , init
  , update
  , view
  )

import App.AppConfig exposing (..)
import App.Products.Product as P
import App.Products.Requests as P
import Data.External exposing (..)
import Html exposing (Html)
import UI.Layout exposing (withLayout)
import Debug

type alias App =
  { appConfig : AppConfig
  , products  : External (List P.Product)
  }

type AppMsg = ProductMsg P.ProductMsg

init : AppConfig -> (App, Cmd AppMsg)
init appConfig =
  let initialState = { appConfig = appConfig, products  = NotLoaded }
  in ( initialState
     , Cmd.map ProductMsg (P.getProducts appConfig)
     )

update : AppMsg -> App -> (App, Cmd AppMsg)
update msg app =
  case msg of
    ProductMsg (P.FetchProductsSucceeded products) -> ({ app | products = Loaded products }, Cmd.none)
    ProductMsg (P.FetchProductsFailed err) -> Debug.log ("Error: " ++ toString err) ({ app | products = LoadedWithError "Failed to load products!" }, Cmd.none)

view : App -> Html a
view app = case app.products of
  NotLoaded             -> loadingView app
  Loaded products       -> productsView products
  LoadedWithError error -> errorView error

loadingView : App -> Html a
loadingView app = withLayout
  [ Html.div [] [ Html.text ("Hi there " ++ (app.appConfig.user)) ] ]

productsView : List P.Product -> Html a
productsView products = withLayout
  [ Html.div [] [ Html.text ("You have " ++ (toString <| List.length products) ++ " products!") ]
  , Html.ul [] (List.map (\product -> Html.li [] [ Html.text (toString product) ]) products)
  ]

errorView : String -> Html a
errorView error = withLayout
  [ Html.div [] [ Html.text ("Oh no! " ++ error) ] ]
