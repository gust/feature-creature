module App.Products.Requests exposing
  ( getProducts
  , createProduct
  )

import App.AppConfig        exposing (..)
import App.Products.Product as Product exposing (Product, ProductMsg (..))
import App.Products.Repository as Repository exposing (Repository)
import Json.Decode as Json  exposing ((:=), maybe)
import Http as Http         exposing (..)
import Task as Task         exposing (..)
import Utils.Http

getProducts : AppConfig -> Cmd ProductMsg
getProducts appConfig =
  Http.get parseProducts (productsUrl appConfig)
    |> Task.perform FetchProductsFailed FetchProductsSucceeded

createProduct : AppConfig -> Repository -> Cmd ProductMsg
createProduct appConfig repository =
  Utils.Http.jsonPostRequest (productsUrl appConfig) (Repository.encodeRepository repository)
    |> Http.send Http.defaultSettings
    |> Http.fromJson parseProduct
    |> Task.perform CreateProductFailed CreateProductSucceeded


parseProducts : Json.Decoder (List Product)
parseProducts = parseProduct |> Json.list

parseProduct : Json.Decoder Product
parseProduct =
  Json.object2
    Product.init
    ("id"   := Json.int)
    ("name" := Json.string)

productsUrl : AppConfig -> String
productsUrl appConfig = appConfig.productsApiPath ++ "/api/products"
