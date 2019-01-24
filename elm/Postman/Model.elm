module Postman.Model exposing (..)

import Json.Decode exposing (..)

import Builder.Model as Builder
import Builder.Method as Builder
import Tree.Model as Tree

type alias Model = Maybe Tree.Tree

decodePostman : String -> Result Error Tree.Tree
decodePostman str =
  decodeString postmanCollectionToTreeDecoder str

postmanCollectionToTreeDecoder : Decoder Tree.Tree
postmanCollectionToTreeDecoder =
  let
    root name requests = Tree.Folder name True requests
    filesDecoder : Decoder (List Tree.Node)
    filesDecoder = field "item" (list fileDecoder)
    fileDecoder : Decoder Tree.Node
    fileDecoder = map2 Tree.File fileNameDecoder builderDecoder
    fileNameDecoder : Decoder String
    fileNameDecoder =  (field "name" string)
    rootDecoder : Decoder Tree.Node
    rootDecoder = map3 Tree.Folder collectionNameDecoder (succeed True) filesDecoder
  in
    map (\folder -> [folder]) rootDecoder

collectionNameDecoder : Decoder String
collectionNameDecoder =
  field "info" (field "name" string)

methodDecoder : Decoder Builder.Method
methodDecoder =
  let
    decode : Maybe Builder.Method -> Decoder Builder.Method
    decode mMethod =
      case mMethod of
        Nothing -> fail "cannot decode method"
        Just method -> succeed method
  in
    string
      |> map Builder.fromString
      |> andThen decode

builderDecoder : Decoder Builder.Model
builderDecoder =
  map8 Builder.Model
    (field "name" string)
    (at ["request", "url"] string)
    (succeed "http")
    (at ["request", "method"] methodDecoder)
    (at ["request", "header"] (succeed []))
    (at ["request", "body"] (succeed ""))
    (succeed { url = True, headers = True})
    (succeed Nothing)
