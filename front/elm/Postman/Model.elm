module Postman.Model exposing (..)

import Json.Decode exposing (..)

import BuilderApp.Builder.Model as Builder
import BuilderApp.Builder.Method as Builder
import BuilderApp.BuilderTree.Model as BuilderTree

type alias Model = Maybe BuilderTree.BuilderTree

decodePostman : String -> Result Error BuilderTree.BuilderTree
decodePostman str =
  decodeString postmanCollectionToBuilderTreeDecoder str

postmanCollectionToBuilderTreeDecoder : Decoder BuilderTree.BuilderTree
postmanCollectionToBuilderTreeDecoder =
  let
    root : String -> BuilderTree.BuilderTree -> BuilderTree.Node
    root name requests = BuilderTree.Folder { name = name, open = True, children = requests, showRenameInput = False }
    filesDecoder : Decoder (List BuilderTree.Node)
    filesDecoder = field "item" (list fileDecoder)
    fileDecoder : Decoder BuilderTree.Node
    fileDecoder = map2 (\name builder -> BuilderTree.File { name = name, builder = builder, showRenameInput = False, isSaved = True }) fileNameDecoder builderDecoder
    fileNameDecoder : Decoder String
    fileNameDecoder =  (field "name" string)
    rootDecoder : Decoder BuilderTree.Node
    rootDecoder = map2 (\name children -> BuilderTree.Folder { name = name, open = True, children = children, showRenameInput = False }) collectionNameDecoder filesDecoder
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
  map7 Builder.Model
    (field "name" string)
    (at ["request", "url"] string)
    (succeed "http")
    (at ["request", "method"] methodDecoder)
    (at ["request", "header"] (succeed []))
    (at ["request", "body"] (succeed ""))
    (succeed Nothing)
