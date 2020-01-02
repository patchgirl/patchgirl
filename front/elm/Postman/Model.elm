module Postman.Model exposing (..)

import Json.Decode exposing (..)

import BuilderApp.Model as BuilderApp
import Api.Generated as Client

type alias Model = Maybe (List BuilderApp.RequestNode)

{-decodePostman : String -> Result Error (List BuilderApp.RequestNode)
decodePostman str =
  decodeString postmanCollectionToBuilderTreeDecoder str
-}
{-postmanCollectionToBuilderTreeDecoder : Decoder (List BuilderApp.RequestNode)
postmanCollectionToBuilderTreeDecoder =
  let
    root : String -> BuilderApp.RequestNode -> BuilderApp.RequestNode
    root name requests =
        BuilderApp.RequestFolder
            { children = [ requests ]
            , name = name
            , open = True
            , showRenameInput = False
            }
    filesDecoder : Decoder (List BuilderApp.RequestNode)
    filesDecoder = field "item" (list fileDecoder)
    fileDecoder : Decoder BuilderApp.RequestNode
    fileDecoder = map2 (\name builder -> BuilderApp.RequestFile
                            { name = name
                            , builder = builder
                            , showRenameInput = False
                            , isSaved = True }
                       ) fileNameDecoder builderDecoder
    fileNameDecoder : Decoder String
    fileNameDecoder =  (field "name" string)
    rootDecoder : Decoder BuilderApp.RequestNode
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

methodDecoder : Decoder Client.Method
methodDecoder =
  let
    decode : Maybe Client.Method -> Decoder Client.Method
    decode mMethod =
      case mMethod of
        Nothing -> fail "cannot decode method"
        Just method -> succeed method
  in
    string
      |> map Builder.fromString
      |> andThen decode

builderDecoder : Decoder (Builder.Model a)
builderDecoder =
  map6 Builder.Model
    (field "name" string)
    (at ["request", "url"] string)
    (at ["request", "method"] methodDecoder)
    (at ["request", "header"] (succeed []))
    (at ["request", "body"] (succeed ""))
    (succeed Nothing)
-}
