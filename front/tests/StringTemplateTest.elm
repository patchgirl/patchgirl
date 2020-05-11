module StringTemplateTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import StringTemplate exposing (..)
import Application.Type exposing(..)
import Parser as P


type alias ParserTest error =
    { message : String
    , input : String
    , expect : Result error StringTemplate
    }

templateStringTests : List (ParserTest (List P.DeadEnd))
templateStringTests =
    [ { message = "empty string"
      , input = ""
      , expect = Ok <| [ Sentence "" ]
      }
    , { message = "one char"
      , input = "a"
      , expect = Ok <| [ Sentence "a", Sentence "" ]
      }
    , { message = "real sentence"
      , input = "hello there! How are you?"
      , expect = Ok <| [ Sentence "hello there! How are you?", Sentence "" ]
      }
    , { message = "template string"
      , input = "{{hello}}"
      , expect = Ok <| [ Key "hello", Sentence "" ]
      }
    , { message = "template string with spaces"
      , input = " {{ hello  }}  "
      , expect = Ok <| [ Sentence " ", Key "hello", Sentence "  ", Sentence "" ]
      }
    , { message = "weird keys"
      , input = "{{a~:#*<>-()[]!@}}"
      , expect = Ok <| [ Key "a~:#*<>-()[]!@", Sentence "" ]
      }
    , { message = "inner keys"
      , input = "{{a{{b}}c}}"
      , expect = Ok <| [ Sentence "" ]
      }
    , { message = "template string with many words"
      , input = "{{hello john}}"
      , expect = Ok <| [ Sentence "" ]
      }
    , { message = "multiple keys"
      , input = "Hello {{John}}, how are you {{date}}?"
      , expect = Ok <| [ Sentence "Hello ", Key "John", Sentence ", how are you ", Key "date", Sentence "?", Sentence "" ]
      }
    ]

suite : Test
suite =
    describe "simple strings" <| List.map checkStringTemplateParser templateStringTests


checkStringTemplateParser : ParserTest (List P.DeadEnd) -> Test
checkStringTemplateParser { message, input, expect } =
    test message <|
        \_ ->
            case P.run stringTemplateParser input of
                Ok _ as ok ->
                    Expect.equal ok expect

                Err err ->
                    Expect.equal (Err err) expect
