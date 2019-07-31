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
      | selectedWorkspaceIndex : Maybe Int
    }

update : Msg -> Model a -> List String -> Model a
update msg model workspaceNames =
    case msg of
        Select idx ->
            case List.getAt idx workspaceNames of
                Just _ ->
                    { model | selectedWorkspaceIndex = Just idx }
                Nothing ->
                    { model | selectedWorkspaceIndex = Nothing }

view : Model a -> List String -> Html Msg
view model workspaceNames =
    select [ style "align-self" "flex-start", on "change" (Json.map Select targetValueIntParse) ]
        (List.indexedMap entryView workspaceNames)

entryView : Int -> String -> Html Msg
entryView idx workspaceName =
    option [ Html.Attributes.value (String.fromInt idx) ] [ text workspaceName ]
