module App.Products.Requests exposing ( getProducts )

import App.AppConfig        exposing (..)
import App.Products.Product exposing (Product, ProductMsg (..), init)
import Json.Decode as Json  exposing ((:=))
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
    init
    ("id"   := Json.int)
    ("name" := Json.string)

productsUrl : AppConfig -> String
productsUrl appConfig = appConfig.productsApiPath ++ "/api/products"

