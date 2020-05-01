module TangoScript.ParserTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import TangoScript.Parser exposing(..)
import Parser as P


-- * expr


-- ** int


intTests : List (ParserTest (List P.DeadEnd) Expr)
intTests =
    [ { message = "parse one digit integer"
      , input = "1"
      , expect = Ok <| LInt 1
      }
    , { message = "parse multi digits integer"
      , input = "123"
      , expect = Ok <| LInt 123
      }
    ]


-- ** bool


boolTests : List (ParserTest (List P.DeadEnd) Expr)
boolTests =
    [ { message = "parse true value"
      , input = "true"
      , expect = Ok <| LBool True
      }
    , { message = "parse false value"
      , input = "false"
      , expect = Ok <| LBool False
      }
    , { message = "don't parse string that looks like boolean"
      , input = "falsey"
      , expect = Ok <| Var "falsey"
      }
    , { message = "don't parse case doesn't match"
      , input = "falsE"
      , expect = Ok <| Var "falsE"
      }
    ]


-- ** string


stringTests : List (ParserTest (List P.DeadEnd) Expr)
stringTests =
    [ { message = "parse simple string"
      , input = """ "foo" """
      , expect = Ok <| LString "foo"
      }
    , { message = "parse empty string"
      , input = """ "" """
      , expect = Ok <| LString ""
      }
    , { message = "parse weird char string"
      , input = """ "aA1&/[]^-" """
      , expect = Ok <| LString "aA1&/[]^-"
      }
    ]


-- ** var


varTests : List (ParserTest (List P.DeadEnd) Expr)
varTests =
    [ { message = "parse var"
      , input = "yes"
      , expect = Ok <| Var "yes"
      }
    , { message = "dont parse var that start with capital case"
      , input = "Yes"
      , expect = Err []
      }
    , { message = "dont parse var that start with number"
      , input = "1yes"
      , expect = Ok <| LInt 1
      }
    , { message = "parse var with weird format"
      , input = "yes01T_iset"
      , expect = Ok <| Var "yes01T_iset"
      }
    , { message = "parse only var before hyphen"
      , input = "foo-bar"
      , expect = Ok <| Var "foo"
      }
    ]


-- ** get


getTests : List (ParserTest (List P.DeadEnd) Expr)
getTests =
    [ { message = "parse simple `get`"
      , input = """get("a")"""
      , expect = Ok <| Get "a"
      }
    , { message = "parse `get`"
      , input = """get ( " a " ) """
      , expect = Ok <| Get " a "
      }
    ]


-- ** get


responseAsStringTests : List (ParserTest (List P.DeadEnd) Expr)
responseAsStringTests =
    [ { message = "parse simple `responseAsString`"
      , input = "httpResponseBodyAsString"
      , expect = Ok <| HttpResponseBodyAsString
      }
    ]


-- ** eq


eqTests : List (ParserTest (List P.DeadEnd) Expr)
eqTests =
    [ { message = "parse `eq`"
      , input = "1 == 2"
      , expect = Ok <| Eq (LInt 1) (LInt 2)
      }
    ]


-- ** add


addTests : List (ParserTest (List P.DeadEnd) Expr)
addTests =
    [ { message = "parse `+`"
      , input = "1 + 2"
      , expect = Ok <| Add (LInt 1) (LInt 2)
      }
    ]


-- * proc


-- ** let


letTests : List (ParserTest (List P.DeadEnd) Proc)
letTests =
    [ { message = "parse simple let"
      , input = "var a = 1"
      , expect = Ok <| Let "a" (LInt 1)
      }
    ]


-- ** assertEqual


assertEqualTests : List (ParserTest (List P.DeadEnd) Proc)
assertEqualTests =
    [ { message = "parse simple assertEqual"
      , input = "assertEqual(1,1)"
      , expect = Ok <| AssertEqual (LInt 1) (LInt 1)
      }
    , { message = "parse assertEqual with spaces"
      , input = "assertEqual ( 1 , 1 ) "
      , expect = Ok <| AssertEqual (LInt 1) (LInt 1)
      }
    , { message = "parse assertEqual with int and bool as param"
      , input = "assertEqual( 1 , true )"
      , expect = Ok <| AssertEqual (LInt 1) (LBool True)
      }
    ]


-- ** set


setTests : List (ParserTest (List P.DeadEnd) Proc)
setTests =
    [ { message = "parse simple `set`"
      , input = """set("a", 1)"""
      , expect = Ok <| Set "a" (LInt 1)
      }
    , { message = "parse `set`"
      , input = """set("a", true)"""
      , expect = Ok <| Set "a" (LBool True)
      }
    ]



-- * tango


tangoTests : List (ParserTests (List P.DeadEnd) TangoAst)
tangoTests =
    [ { message = "parse simple statement"
      , input = "assertEqual(1,1);"
      , expect = Ok <| [ AssertEqual (LInt 1) (LInt 1) ]
      }
    , { message = "dont parse statement that doesnt end with semi-colon"
      , input = "assertEqual(1,1)"
      , expect = Err [{ col = 17, problem = P.ExpectingSymbol ";", row = 1 }]
      }
    , { message = "dont parse expression"
      , input = "1;"
      , expect = Ok []
      }
    , { message = "parse more complex statements"
      , input = """
                 var a = 1;
                 var b = 2;
                 assertEqual(a, b);
                """
      , expect = Ok [ Let "a" (LInt 1)
                    , Let "b" (LInt 2)
                    , AssertEqual (Var "a") (Var "b")
                    ]
      }
    ]


-- * suite


suite : Test
suite =
    describe "Parser module"
        [ describe "ExprParser"
            [ describe "LInt" <| List.map checkExprParser intTests
            , describe "LBool" <| List.map checkExprParser boolTests
            , describe "LString" <| List.map checkExprParser stringTests
            , describe "Var" <| List.map checkExprParser varTests
            , describe "Get" <| List.map checkExprParser getTests
            , describe "ResponseAsString" <| List.map checkExprParser responseAsStringTests
            , describe "Eq" <| List.map checkExprParser eqTests
            , describe "Add" <| List.map checkExprParser addTests
            ]
        , describe "ProcParser"
            [ describe "Let" <| List.map checkProcParser letTests
            , describe "AssertEqual" <| List.map checkProcParser assertEqualTests
            , describe "Set" <| List.map checkProcParser setTests
            ]
        , describe "TangoParser"
            <| List.map checkTangoAstParser tangoTests
        ]


-- * util


type alias ParserTest error a =
    { message : String
    , input : String
    , expect : Result error a
    }

checkExprParser : ParserTest (List P.DeadEnd) Expr -> Test
checkExprParser { message, input, expect } =
    test message <|
        \_ ->
            case P.run exprParser input of
                Ok _ as ok ->
                    Expect.equal ok expect

                Err _ ->
                    Expect.equal (Err []) expect

checkProcParser : ParserTest (List P.DeadEnd) Proc -> Test
checkProcParser { message, input, expect } =
    test message <|
        \_ ->
            case P.run procParser input of
                Ok _ as ok ->
                    Expect.equal ok expect

                Err _ ->
                    Expect.equal (Err []) expect

type alias ParserTests error a =
    { message : String
    , input : String
    , expect : Result error a
    }

checkTangoAstParser : ParserTests (List P.DeadEnd) TangoAst -> Test
checkTangoAstParser { message, input, expect } =
    test message <|
        \_ ->
            let
                a : Result (List P.DeadEnd) TangoAst
                a = P.run tangoParser input
            in
            Expect.equal (P.run tangoParser input) expect
