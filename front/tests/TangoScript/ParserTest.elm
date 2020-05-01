module TangoScript.ParserTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import TangoScript.Parser exposing(..)
import Parser as P


suite : Test
suite =
    describe "Parser module"


-- * mExpr parser


        [ describe "mExprParser"
            [ test "don't parse when there's nothing to parse" <|
                  \_ ->
                  let
                      inputWithNewlines = """


                                          """
                      inputs = [ "", " ", inputWithNewlines ]
                      expectedRes = [ Ok Nothing, Ok Nothing, Ok Nothing ]
                  in
                  Expect.equal (List.map (P.run mExprParser) inputs) expectedRes
            ]


-- * expr parser


        , describe "ExprParser"


-- ** litteral


            [ test "LInt" <|
                  \_ ->
                  let
                      input = "1;"
                      expectedRes = Lit (LInt 1)
                  in
                  Expect.equal (P.run exprParser input) (Ok expectedRes)

            , test "LBool" <|
                  \_ ->
                  let
                      inputs = ["true;", "false;", " true  ;"]
                      expectedRes =
                          [ Ok (Lit (LBool True))
                          , Ok (Lit (LBool False))
                          , Ok (Lit (LBool True))
                          ]
                  in
                  Expect.equal expectedRes (List.map (P.run exprParser) inputs)


-- ** bin op


            , test "add op" <|
                  \_ ->
                  let
                      inputs = ["1+1;", "1 + 1;", "1 + true ;"]
                      expectedRes =
                          [ Ok <| Op Add (Lit (LInt 1)) (Lit (LInt 1))
                          , Ok <| Op Add (Lit (LInt 1)) (Lit (LInt 1))
                          , Ok <| Op Add (Lit (LInt 1)) (Lit (LBool True))
                          ]

                  in
                  Expect.equal (List.map (P.run exprParser) inputs) expectedRes

-- ** assert equal


            , test "assert equal" <|
                  \_ ->
                  let
                      inputs =
                          [ "assertEqual(1,1);"
                          , "assertEqual( 1 , 1 );"
                          , "assertEqual( 1 , true );"
                          , "assertEqual( 1 , 1 + 1 );"
                          ]
                      expectedRes =
                          [ Ok <| AssertEqual (Lit (LInt 1)) (Lit (LInt 1))
                          , Ok <| AssertEqual (Lit (LInt 1)) (Lit (LInt 1))
                          , Ok <| AssertEqual (Lit (LInt 1)) (Lit (LBool True))
                          , Ok <| AssertEqual (Lit (LInt 1)) (Op Add (Lit (LInt 1)) (Lit (LInt 1)))
                          ]

                  in
                  Expect.equal (List.map (P.run exprParser) inputs) expectedRes



-- ** var


            , test "var" <|
                  \_ ->
                  let
                      inputs =
                          [ "foo"
                          ]
                      expectedRes =
                          [ Ok <| Var "foo"
                          ]

                  in
                  Expect.equal (List.map (P.run exprParser) inputs) expectedRes


-- ** var assoc


            , test "var assoc" <|
                  \_ ->
                  let
                      inputs =
                          [ "let a = 1;"
                          , "let a = 1 + 1;"
                          , "let a = 1 + false;"
                          , "let a = 1 + foo;"
                          ]
                      expectedRes =
                          [ Ok <| Let "a" (Lit (LInt 1))
                          , Ok <| Let "a" (Op Add (Lit (LInt 1)) (Lit (LInt 1)))
                          , Ok <| Let "a" (Op Add (Lit (LInt 1)) (Lit (LBool False)))
                          , Ok <| Let "a" (Op Add (Lit (LInt 1)) (Var "foo"))
                          ]

                  in
                  Expect.equal (List.map (P.run exprParser) inputs) expectedRes

                      ]


-- * main parser


        , describe "MainParser"


-- ** parser


            [ test "app" <|
                  \_ ->
                  let
                      inputs =
                          [ """
                            1 ; 1;
                            """
                          , """
                            1 ; 1
                             ;
                            """
                          , """
                             let a = 1;
                             let b = 2;
                             a + b;
                            """
                          ]
                      expectedRes =
                          [ Ok <| [ (Lit (LInt 1))
                                  , (Lit (LInt 1))
                                  ]
                          , Ok <| [ (Lit (LInt 1))
                                  , (Lit (LInt 1))
                                  ]
                          , Ok <| [ Let "a" (Lit (LInt 1))
                                  , Let "b" (Lit (LInt 2))
                                  , Op Add (Var "a") (Var "b")
                                  ]
                          ]

                  in
                  Expect.equal (List.map (P.run parser) inputs) expectedRes

            ]
        ]
