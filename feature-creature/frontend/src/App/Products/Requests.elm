module App.Products.Requests exposing
  ( getProducts
  , getRepositories
  )

import App.AppConfig        exposing (..)
import App.Products.Product as Product exposing (Product, ProductMsg (..), init)
import App.Products.Repository as Repository exposing (Repository, RepositoryMsg (..), init)
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
    Product.init
    ("id"   := Json.int)
    ("name" := Json.string)

productsUrl : AppConfig -> String
productsUrl appConfig = appConfig.productsApiPath ++ "/api/products"


getRepositories : AppConfig -> Cmd RepositoryMsg
getRepositories appConfig =
  Http.get parseRepositories (repositoriesUrl appConfig)
    |> Task.perform FetchRepositoriesFailed FetchRepositoriesSucceeded

parseRepositories : Json.Decoder (List Product)
parseRepositories = parseRepository |> Json.list

parseRepository : Json.Decoder Repository
parseRepository =
  Json.object2
    Repository.init
    ("id"   := Json.int)
    ("name" := Json.string)

repositoriesUrl : AppConfig -> String
repositoriesUrl appConfig = appConfig.productsApiPath ++ "/api/repositories"
