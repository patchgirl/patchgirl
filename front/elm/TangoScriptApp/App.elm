module TangoScriptApp.App exposing (..)

import Application.Type exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Util exposing (..)
import TangoScript.Parser exposing(..)
import Parser as P


-- * model


type alias Model a =
    { a
        | script : String
    }


-- ** message


type Msg
    = EditScript String
    | DoNothing


-- ** update


update : Msg -> Model a -> ( Model a, Cmd Msg )
update msg model =
    case msg of
        EditScript newScript ->
            let
                newModel = { model | script = newScript }
            in
            (newModel, Cmd.none)

        DoNothing ->
            (model, Cmd.none)

-- ** view


view : Model a -> Element Msg
view model =
    let
        astResult : Result (List P.DeadEnd) TangoAst
        astResult =
            P.run tangoParser model.script
    in
    column [ width fill, padding 10, spacing 10, centerX ]
        [ Input.multiline []
            { onChange = EditScript
            , text = model.script
            , placeholder = Just <| Input.placeholder [] (text "test some tangoScript!")
            , label = Input.labelHidden "TangoScript: "
            , spellcheck = False
            }
        , Input.multiline []
            { onChange = always DoNothing
            , text =
                case astResult of
                    Ok ast -> astAsString ast
                    Err errors -> P.deadEndsToString errors

            , placeholder = Just <| Input.placeholder [] (text "test some tangoScript!")
            , label = Input.labelHidden "TangoScript: "
            , spellcheck = False
            }
        ]
