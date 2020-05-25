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
        [ row [ width fill, spacing 10 ]
              [ Input.multiline [ height fill ]
                    { onChange = EditScript
                    , text = model.script
                    , placeholder = Just <| Input.placeholder [] (text "")
                    , label = Input.labelAbove [] (text "Tangoscript:")
                    , spellcheck = False
                    }
              , Input.multiline []
                  { onChange = always DoNothing
                  , text = tangoScriptHelp
                  , placeholder = Nothing
                  , label = Input.labelAbove [] (text "Example of some Tangoscript:")
                  , spellcheck = False
                  }
              ]
        , column [ width fill, centerX, spacing 10 ]
            (case astResult of
              Ok ast ->
                  [ astView (astAsString ast)
                  , compatibilityView ast
                  ]

              Err errors ->
                  [ astView (P.deadEndsToString errors)
                  ])
        ]

astView : String -> Element Msg
astView ast =
    Input.multiline [ width fill, centerX ]
        { onChange = always DoNothing
        , text = ast
        , placeholder = Nothing
        , label = Input.labelAbove [] (text "Abstract syntax tree:")
        , spellcheck = False
        }

compatibilityView : TangoAst -> Element Msg
compatibilityView tangoAst =
    Input.multiline [ width fill, centerX ]
        { onChange = always DoNothing
        , text = "" -- check tangoAst |> List.map showError |> String.join "\n"
        , placeholder = Nothing
        , label = Input.labelAbove [] (text "Errors:")
        , spellcheck = False
        }

tangoScriptHelp : String
tangoScriptHelp = """// test some tangoScript!

// you can assign variable
var theAnswer = 42;

// get variables from the environment
var id = get("id");

// assert result
assertEqual(theAnswer, 42);
"""
