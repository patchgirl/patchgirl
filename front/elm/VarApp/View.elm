module VarApp.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Html.Events as Events
import Json.Decode as Decode

import VarApp.Message exposing (..)
import VarApp.Model exposing (..)

import Util.Maybe as Maybe

view : Model -> Html Msg
view model =
    let
        varViewsWithoutLastDropZone = List.indexedMap (varView model) model.vars |> List.concat
        lastDropZone = varDropableView model (List.length model.vars)
        varViewsWithDropZones = varViewsWithoutLastDropZone ++ [ lastDropZone ]
    in
        div [ id "varApp", class "columns" ]
            [ ul [ class "column" ] varViewsWithDropZones
            , defaultView
            ]

varView : Model -> Int -> (String, String) -> List(Html Msg)
varView model idx var =
  [(varDropableView model idx), (varDragableView idx var)]

varDropableView : Model -> Int -> Html Msg
varDropableView model idx  =
    let
        isZoneNeighbourOfDragged =
            Maybe.exists model.draggedId (\draggedId -> draggedId == idx || draggedId + 1 == idx)
        isAbove = Maybe.exists model.draggedId (\dId -> idx > dId)
        newIdx =
            case isAbove of
                True -> idx - 1
                False -> idx
        onOverClass =
            case model.overZoneId == Just idx of
                True -> "is-over"
                False -> ""
    in
        case isZoneNeighbourOfDragged of
            True ->
                li [ class "dropZone"
                   , class "notDroppable"
                   ] []
            False ->
                li [ onDragOver (DragOver idx)
                   , onDragLeave DragLeave
                   , onDrop (Drop newIdx)
                   , class "dropZone"
                   , class onOverClass
                   ] [ text (String.fromInt (newIdx))]

varDragableView : Int -> (String, String) -> Html Msg
varDragableView idx (varKey, varValue) =
    li [ draggable "true"
       , onDragStart (Drag idx)
       , onDragEnd DragEnd
       , class "varInput"
       ]
       [ input [ id ""
               , placeholder "key"
               , onInput (PromptKey idx)
               , value varKey
               ] []
       , input [ id ""
               , placeholder "value"
               , onInput (PromptValue idx)
               , value varValue
               ] []
       , input [ onClick (DeleteInput idx), type_ "button" ]
           []
       ]

onDragStart msg =
    Events.on "dragstart" <| Decode.succeed msg

onDragEnd msg =
    Events.preventDefaultOn "dragend" <| Decode.succeed (msg, True)

onDragOver msg =
    Events.preventDefaultOn "dragover" <| Decode.succeed (msg, True)

onDragLeave msg =
    Events.preventDefaultOn "dragleave" <| Decode.succeed (msg, True)

onDrop msg =
    Events.preventDefaultOn "drop" <| Decode.succeed (msg, True)

defaultView : Html Msg
defaultView =
  div [ onClick AddNewInput, class "column" ]
      [ span [] [ text "add new var" ]
      ]
