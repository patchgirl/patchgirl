module Window.StateHandler exposing (..)

import Http
import Json.Encode as Json

import Window.Model exposing (..)
import Window.Message exposing (..)

import Tree.Model as Tree
import Builder.Model as Builder
import Builder.Method as Builder

stateEncoder : Tree.Model -> Json.Value
stateEncoder model =
  let
    nodes : Json.Value
    nodes = List.map nodeEncoder model.tree |> toArray
  in
    Json.object [ ("root", nodes) ]

toArray : List Json.Value -> Json.Value
toArray values = Json.list (\a -> a) values

nodeEncoder : Tree.Node -> Json.Value
nodeEncoder node =
  case node of
    Tree.Folder f -> Json.object [ ("folder", folderEncoder f) ]
    Tree.File f -> Json.object [ ("file", fileEncoder f) ]

fileEncoder : Tree.File2 -> Json.Value
fileEncoder file =
  Json.object
    [ ("type", Json.string "file")
    , ("name", Json.string file.name)
    , ("builder", builderEncoder file.builder)
    ]

headerEncoder : Builder.Header -> Json.Value
headerEncoder (key, value) =
  Json.object [ (key, Json.string value) ]

builderEncoder : Builder.Model -> Json.Value
builderEncoder builder =
  Json.object
    [ ("name", Json.string builder.name)
    , ("url", Json.string builder.url)
    , ("scheme", Json.string builder.scheme)
    , ("method", Json.string (Builder.methodToString builder.method))
    , ("headers", List.map headerEncoder builder.headers |> toArray)
    , ("body", Json.string builder.body)
    ]

folderEncoder : Tree.Folder2 -> Json.Value
folderEncoder folder =
  Json.object
    [ ("type", Json.string "folder")
    , ("name", Json.string folder.name)
    , ("open", Json.bool folder.open)
    , ("children", List.map nodeEncoder folder.children |> toArray)
    ]

sendSaveTabRequest : Tree.Model -> Cmd Msg
sendSaveTabRequest model =
  let
    httpRequest = Http.request
      { method = "PUT"
      , headers = []
      , url = "http://localhost:9000"
      , body = Debug.log "state" (stateEncoder model) |> Http.jsonBody
      , expect = Http.expectString SaveTreeResponse
      , timeout = Nothing
      , tracker = Nothing
      }
  in
    httpRequest
