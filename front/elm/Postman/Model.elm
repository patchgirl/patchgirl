module Postman.Model exposing (..)

import Json.Decode exposing (..)

import BuilderApp.Builder.Model as Builder
import BuilderApp.Builder.Method as Builder
import BuilderApp.Model as BuilderApp

type alias Model = Maybe (List BuilderApp.Node)

decodePostman : String -> Result Error (List BuilderApp.Node)
decodePostman str =
  decodeString postmanCollectionToBuilderTreeDecoder str

postmanCollectionToBuilderTreeDecoder : Decoder (List BuilderApp.Node)
postmanCollectionToBuilderTreeDecoder =
  let
    root : String -> BuilderApp.Node -> BuilderApp.Node
    root name requests =
        BuilderApp.RequestFolder
            { children = [ requests ]
            , name = name
            , open = True
            , showRenameInput = False
            }
    filesDecoder : Decoder (List BuilderApp.Node)
    filesDecoder = field "item" (list fileDecoder)
    fileDecoder : Decoder BuilderApp.Node
    fileDecoder = map2 (\name builder -> BuilderApp.RequestFile
                            { name = name
                            , builder = builder
                            , showRenameInput = False
                            , isSaved = True }
                       ) fileNameDecoder builderDecoder
    fileNameDecoder : Decoder String
    fileNameDecoder =  (field "name" string)
    rootDecoder : Decoder BuilderApp.Node
    rootDecoder = map2 (\name children -> BuilderApp.RequestFolder
                            { name = name
                            , open = True
                            , children = children
                            , showRenameInput = False }
                       ) collectionNameDecoder filesDecoder
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
  map6 Builder.Model
    (field "name" string)
    (at ["request", "url"] string)
    (at ["request", "method"] methodDecoder)
    (at ["request", "header"] (succeed []))
    (at ["request", "body"] (succeed ""))
    (succeed Nothing)
