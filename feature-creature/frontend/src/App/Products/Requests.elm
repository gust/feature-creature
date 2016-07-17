module App.Products.Requests exposing
  ( getProducts
  , getRepositories
  )

import App.AppConfig        exposing (..)
import App.Products.Product as Product exposing (Product, ProductMsg (..))
import App.Products.Repository as Repository exposing (Repository, RepositoryOwner, RepositoryMsg (..))
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


getRepositories : AppConfig -> Cmd RepositoryMsg
getRepositories appConfig =
  Http.get parseRepositories (repositoriesUrl appConfig)
    |> Task.perform FetchRepositoriesFailed FetchRepositoriesSucceeded

parseRepositories : Json.Decoder (List Repository)
parseRepositories = parseRepository |> Json.list

parseRepository : Json.Decoder Repository
parseRepository =
  Json.object8
    Repository
      ("id"   := Json.int)
      ("name" := Json.string)
      ("url" := Json.string)
      ("htmlUrl" := Json.string)
      (maybe ("sshUrl" := Json.string))
      (maybe ("cloneUrl" := Json.string))
      ("hooksUrl" := Json.string)
      ("owner" := parseRepositoryOwner)

parseRepositoryOwner : Json.Decoder RepositoryOwner
parseRepositoryOwner =
  Json.object4
    RepositoryOwner
      ("id"   := Json.int)
      ("name" := Json.string)
      ("url" := Json.string)
      ("avatarUrl" := Json.string)

repositoriesUrl : AppConfig -> String
repositoriesUrl appConfig = appConfig.productsApiPath ++ "/api/repositories"
