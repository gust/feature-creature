module App.Products.Requests exposing
  ( getProducts
  )

import App.AppConfig        exposing (..)
import App.Products.Product as Product exposing (Product, ProductMsg (..))
import Json.Decode as Json  exposing ((:=), maybe)
import Http as Http         exposing (..)
import Task as Task         exposing (..)

getProducts : AppConfig -> Cmd ProductMsg
getProducts appConfig =
  Http.get parseProducts (productsUrl appConfig)
    |> Task.perform FetchProductsFailed FetchProductsSucceeded

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
