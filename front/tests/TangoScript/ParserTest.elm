module TangoScript.ParserTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import TangoScript.Parser exposing(..)
import TangoScript.DoubleQuoteString exposing(..)
import Parser as P
import Application.Type exposing (..)
import Dict exposing (Dict)


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
      , expect = Ok <| LVar "falsey"
      }
    , { message = "don't parse case doesn't match"
      , input = "falsE"
      , expect = Ok <| LVar "falsE"
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


-- ** list


listTests : List (ParserTest (List P.DeadEnd) Expr)
listTests =
    [ { message = "parse empty list"
      , input = """ [] """
      , expect = Ok <| LList []
      }
    , { message = "parse one element list"
      , input = """ [1] """
      , expect = Ok <| LList [ LInt 1 ]
      }
    , { message = "parse multiple elements list"
      , input = """ [1, 2,3,   6] """
      , expect = Ok <| LList [ LInt 1, LInt 2, LInt 3, LInt 6 ]
      }
    , { message = "parse heterogeneous list"
      , input = """ [1, true] """
      , expect = Ok <| LList [ LInt 1, LBool True ]
      }
    , { message = "parse list of list"
      , input = """ [1, [2]] """
      , expect = Ok <| LList [ LInt 1, LList [ LInt 2 ] ]
      }
    , { message = "fail to parse trailing commma list"
      , input = """ [1, 2,] """
      , expect = Err []
      }
    , { message = "fail to parse unclosed list"
      , input = """ [1"""
      , expect = Err []
      }
    , { message = "access list from var"
      , input = """ a[0]"""
      , expect = Ok <| LAccessOp (LVar "a") (LInt 0)
      }
    , { message = "access list from list"
      , input = """ [1][0]"""
      , expect = Ok <| LAccessOp (LList [ LInt 1 ]) (LInt 0)
      }
    , { message = "access list with some var"
      , input = """ [1][myIndex]"""
      , expect = Ok <| LAccessOp (LList [ LInt 1 ]) (LVar "myIndex")
      }
    , { message = "double access list with some var"
      , input = """ a[0][1]"""
      , expect = Ok <| LAccessOp (LAccessOp (LVar "a") (LInt 0)) (LInt 1)
      }
    ]


-- ** json


jsonTests : List (ParserTest (List P.DeadEnd) Expr)
jsonTests =
    [ { message = "parse JInt"
      , input = """ {"a": 1} """
      , expect = Ok <| LJson <| JObject <| Dict.fromList [ ("a", JInt 1) ]
      }
    , { message = "parse JString"
      , input = """ {"a": "b"} """
      , expect = Ok <| LJson <| JObject <| Dict.fromList [ ("a", JString "b") ]
      }
    , { message = "parse JBool"
      , input = """ {"a": true } """
      , expect = Ok <| LJson <| JObject <| Dict.fromList [ ("a", JBool True) ]
      }
    , { message = "parse JFloat"
      , input = """ {"a": 1.2 } """
      , expect = Ok <| LJson <| JObject <| Dict.fromList [ ("a", JFloat 1.2) ]
      }
    , { message = "parse JArray"
      , input = """ {"a": [1] } """
      , expect = Ok <| LJson <| JObject <| Dict.fromList [ ("a", JArray [ JInt 1 ]) ]
      }
    , { message = "parse complex JArray"
      , input = """ {"a": [ {"a": 1 }, "e" ] } """
      , expect = Ok (LJson (JObject (Dict.fromList [("a",JArray [JObject (Dict.fromList [("a",JInt 1)]),JString "e"])])))
      }
    , { message = "parse recursive json"
      , input = """ {"a": { "b": 2 }} """
      , expect = Ok <| LJson <|
                 JObject <| Dict.fromList [ ("a", JObject <| Dict.fromList [ ("b", JInt 2) ])]
      }
    ]


-- ** var


varTests : List (ParserTest (List P.DeadEnd) Expr)
varTests =
    [ { message = "parse var"
      , input = "yes"
      , expect = Ok <| LVar "yes"
      }
    , { message = "dont parse var that start with capital case"
      , input = "Yes"
      , expect = Err []
      }
    , { message = "parse var with weird format"
      , input = "yes01T_iset"
      , expect = Ok <| LVar "yes01T_iset"
      }
    , { message = "parse only var before hyphen"
      , input = "foo-bar"
      , expect = Ok <| LVar "foo"
      }
    ]


-- ** char


charTests : List (ParserTest (List P.DeadEnd) Char)
charTests =
    [ { message = "parse char `f`"
      , input = "f"
      , expect = Ok 'f'
      }
    , { message = "parse char `e`"
      , input = "e"
      , expect = Ok 'e'
      }
    ]


-- ** get


getTests : List (ParserTest (List P.DeadEnd) Expr)
getTests =
    [ { message = "parse simple `get`"
      , input = """get("a")"""
      , expect = Ok <| LFetch "a"
      }
    , { message = "parse `get`"
      , input = """get ( " a " ) """
      , expect = Ok <| LFetch " a "
      }
    ]


-- ** http response as string


responseAsStringTests : List (ParserTest (List P.DeadEnd) Expr)
responseAsStringTests =
    [ { message = "parse simple `httpResponseBodyAsString`"
      , input = "httpResponseBodyAsString"
      , expect = Ok <| LHttpResponseBodyAsString
      }
    ]


-- ** http response status


responseStatusAsStringTests : List (ParserTest (List P.DeadEnd) Expr)
responseStatusAsStringTests =
    [ { message = "parse simple `httpResponseStatus`"
      , input = "httpResponseStatus"
      , expect = Ok <| LHttpResponseStatus
      }
    ]


-- ** double quote string


doubleQTests : List (ParserTest (List P.DeadEnd) String)
doubleQTests =
    [ { message = "unfinished double quoted string"
      , input = """"a """
      , expect = Err []
      }
    , { message = "unfinished double quoted string 2"
      , input = """
                 set("
                """
      , expect = Err []
      }
    ]


-- ** eq


eqTests : List (ParserTest (List P.DeadEnd) Expr)
eqTests =
    [ { message = "parse `eq`"
      , input = "1 == 2"
      , expect = Ok <| LEq (LInt 1) (LInt 2)
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


-- ** assertLEqual


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
      , expect = Err []
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
                    , AssertEqual (LVar "a") (LVar "b")
                    ]
      }
    , { message = "parse more complex statements with strings"
      , input = """
                 var a = "1";
                 var b = "2";
                 assertEqual(a, b);
                """
      , expect = Ok [ Let "a" (LString "1")
                    , Let "b" (LString "2")
                    , AssertEqual (LVar "a") (LVar "b")
                    ]
      }
    , { message = "parse statements with random newlines"
      , input = """


                 var a = "1";

                 assertEqual(a, b);


                """
      , expect = Ok [ Let "a" (LString "1")
                    , AssertEqual (LVar "a") (LVar "b")
                    ]
      }
    , { message = "don't parse statements on same line"
      , input = """
                 var a = "1";assertEqual(a, b);
                """
      , expect = Err [ ]
      }
    , { message = "parse httpResponse"
      , input = """
                 var b = postgresResponse[0];
                """
      , expect = Ok [ Let "b" (LAccessOp LPgSimpleResponse (LInt 0))
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
            , describe "LList" <| List.map checkExprParser listTests
            , describe "LJson" <| List.map checkExprParser jsonTests
            , describe "LString" <| List.map checkExprParser stringTests
            , describe "LVar" <| List.map checkExprParser varTests
            , describe "Get" <| List.map checkExprParser getTests
            , describe "ResponseAsString" <| List.map checkExprParser responseAsStringTests
            , describe "ResponseStatusAsString" <| List.map checkExprParser responseStatusAsStringTests
--            , describe "Eq" <| List.map checkExprParser eqTests
            , describe "DQString" <| List.map checkDStringParser doubleQTests
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


-- ** expr


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


-- ** double quote string


checkDStringParser : ParserTest (List P.DeadEnd) String -> Test
checkDStringParser { message, input, expect } =
    test message <|
        \_ ->
            case P.run doubleQuoteString input of
                Ok _ as ok ->
                    Expect.equal ok expect

                Err _ ->
                    Expect.equal (Err []) expect


-- ** proc


checkProcParser : ParserTest (List P.DeadEnd) Proc -> Test
checkProcParser { message, input, expect } =
    test message <|
        \_ ->
            case P.run procParser input of
                Ok _ as ok ->
                    Expect.equal ok expect

                Err _ ->
                    Expect.equal (Err []) expect


-- ** tango


type alias ParserTests error a =
    { message : String
    , input : String
    , expect : Result error a
    }

checkTangoAstParser : ParserTests (List P.DeadEnd) TangoAst -> Test
checkTangoAstParser { message, input, expect } =
    test message <|
        \_ ->
            case parseTangoscript input of
                Ok _ as ok ->
                    Expect.equal ok expect

                Err _ ->
                    Expect.equal (Err []) expect
