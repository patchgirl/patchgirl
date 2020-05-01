module TangoScript.Parser exposing (..)

import Parser as P exposing((|.), (|=), Step, Parser)
import Parser.Expression as P exposing (OperatorTable)
import Set exposing (Set)


-- * reserved keywords


reserved : Set String
reserved =
    Set.fromList
        [ "assertEqual"
        , "var"
        , "true"
        , "false"
        ]


-- * proc


type Proc
    = AssertEqual Expr Expr
    | Let String Expr

procParser : Parser Proc
procParser =
    P.succeed identity
        |. P.spaces
        |= P.oneOf
           [ assertEqualParser
           , letParser
           ]
        |. P.spaces


-- ** let


letParser : Parser Proc
letParser =
    P.succeed Let
        |. P.keyword "var"
        |. P.spaces
        |= variableNameParser
        |. P.spaces
        |. P.symbol "="
        |. P.spaces
        |= (P.lazy <| \_ -> exprParser)


-- ** assert equal


assertEqualParser : Parser Proc
assertEqualParser =
    P.succeed AssertEqual
        |. P.keyword "assertEqual"
        |. P.spaces
        |. P.symbol "("
        |. P.spaces
        |= (P.lazy <| \_ -> exprParser)
        |. P.spaces
        |. P.symbol ","
        |. P.spaces
        |= (P.lazy <| \_ -> exprParser)
        |. P.spaces
        |. P.symbol ")"


-- * expr


type Expr
    = LBool Bool
    | LInt Int
    | Var String


exprParser : Parser Expr
exprParser =
    let
        lIntParser =
            P.succeed LInt
                |= P.int

        lBoolParser =
            P.succeed LBool
                |= P.oneOf [ P.map (always True) (P.keyword "true")
                           , P.map (always False) (P.keyword "false")
                           ]
    in
    P.succeed identity
        |. P.spaces
        |= P.oneOf
           [ lBoolParser
           , lIntParser
           , varParser
           ]
        |. P.spaces


-- * parser


type alias TangoAst = List Proc

tangoParser : Parser TangoAst
tangoParser =
    let
        lineHelper : List Proc -> Parser (Step TangoAst TangoAst)
        lineHelper revStmts =
            P.oneOf
                [ P.succeed (\stmt -> P.Loop (stmt :: revStmts))
                    |= procParser
                    |. P.spaces
                    |. P.symbol ";"
                    |. P.spaces
                , P.succeed ()
                    |> P.map (\_ -> P.Done (List.reverse revStmts))
                ]

    in
    P.succeed identity
        |= P.loop [] lineHelper

-- ** expr

{-
parser : Parser (List Expr)
parser =
    let
        lineHelper : List Expr -> Parser (Step (List Expr) (List Expr))
        lineHelper revStmts =
            P.oneOf
                [ P.succeed (\stmt -> P.Loop (stmt :: revStmts))
                    |= exprParser
                    |. P.spaces
                    |. P.symbol ";"
                    |. P.spaces
                , P.succeed ()
                    |> P.map (\_ -> P.Done (List.reverse revStmts))
                ]

    in
    P.succeed identity
        |= P.loop [] lineHelper


mExprParser : Parser (Maybe Expr)
mExprParser =
    let
        noExprParser : Parser (Maybe Expr)
        noExprParser =
            P.succeed Nothing
                |. P.spaces
                |. P.end
    in
    P.oneOf
        [ noExprParser
        , P.map Just exprParser
        ]

exprParser : Parser Expr
exprParser =
    P.succeed identity
        |. P.spaces
        |= P.oneOf
           [ binOpParser
           , assertEqualParser
           , letParser
           , varParser
           , litParser
           ]
        |. P.spaces
-}

-- ** variable


variableNameParser : Parser String
variableNameParser =
    P.variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = reserved
        }

varParser : Parser Expr
varParser =
    P.map Var variableNameParser


-- ** bin operator

{-
binOpParser : Parser Expr
binOpParser =
    let
        operators : List (List (P.Operator Expr))
        operators =
            [ [ P.infixOperator (\a b -> BinOp Add a b) (P.symbol "+") P.AssocLeft
              ]
            ]

        operandParser : Parser Expr
        operandParser =
            P.oneOf
                [ varParser
                , litParser
                ]
    in
        P.buildExpressionParser operators <|
            P.lazy <| \_ ->
                P.succeed identity
                    |. P.spaces
                    |= P.lazy (\_ -> operandParser)
                    |. P.spaces
-}
