module TangoScript.ParserTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import TangoScript.Parser exposing(..)
import Parser as P


-- * expr


lint : List (ParserTest (List P.DeadEnd) Expr)
lint =
    [ { message = "parse one digit integer"
      , input = "1"
      , expect = Ok <| LInt 1
      }
    , { message = "parse multi digits integer"
      , input = "123"
      , expect = Ok <| LInt 123
      }
    ]

lbool : List (ParserTest (List P.DeadEnd) Expr)
lbool =
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

lvar : List (ParserTest (List P.DeadEnd) Expr)
lvar =
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


-- * proc


letTests : List (ParserTest (List P.DeadEnd) Proc)
letTests =
    [ { message = "parse simple let"
      , input = "var a = 1"
      , expect = Ok <| Let "a" (LInt 1)
      }
    ]

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
            [ describe "LInt" <| List.map checkExprParser lint
            , describe "LBool" <| List.map checkExprParser lbool
            , describe "Var" <| List.map checkExprParser lvar
            ]
        , describe "ProcParser"
            [ describe "Let" <| List.map checkProcParser letTests
            , describe "AssertEqual" <| List.map checkProcParser assertEqualTests
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
