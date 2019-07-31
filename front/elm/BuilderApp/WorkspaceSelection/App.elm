module BuilderApp.WorkspaceSelection.App exposing (..)

import BuilderApp.WorkspaceSelection.Message exposing (Msg(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Events.Extra exposing (targetValueIntParse)
import Json.Decode as Json
import List.Extra as List

type alias Model a =
    { a
      | selectedWorkspaceIdx : Maybe Int
      , workspaceNames : List String
    }

update : Msg -> Model a -> Model a
update msg model =
    case msg of
        Select idx ->
            case List.getAt idx model.workspaceNames of
                Just _ ->
                    { model | selectedWorkspaceIdx = Just idx }
                Nothing ->
                    { model | selectedWorkspaceIdx = Nothing }

view : Model a -> Html Msg
view model =
    select [ style "align-self" "flex-start", on "change" (Json.map Select targetValueIntParse) ]
        (List.indexedMap entryView model.workspaceNames)

entryView : Int -> String -> Html Msg
entryView idx workspaceName =
    option [ Html.Attributes.value (String.fromInt idx) ] [ text workspaceName ]
