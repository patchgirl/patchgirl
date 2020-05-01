module TangoScript.Parser exposing (..)

import Parser as P exposing((|.), (|=), Step, Parser)
import Parser.Expression as P exposing (OperatorTable)
import Set exposing (Set)
import TangoScript.DoubleQuoteString exposing(doubleQuoteString)

-- * reserved keywords


reserved : Set String
reserved =
    Set.fromList
        [ "assertEqual"
        , "var"
        , "get"
        , "set"
        , "httpResponseBodyAsString"
        , "true"
        , "false"
        ]


-- * proc


type Proc
    = AssertEqual Expr Expr
    | Let String Expr
    | Set String Expr

procParser : Parser Proc
procParser =
    P.succeed identity
        |. P.spaces
        |= P.oneOf
           [ assertEqualParser
           , letParser
           , setParser
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


-- ** set


setParser : Parser Proc
setParser =
    P.succeed Set
        |. P.keyword "set"
        |. P.spaces
        |. P.symbol "("
        |. P.spaces
        |= doubleQuoteString
        |. P.spaces
        |. P.symbol ","
        |. P.spaces
        |= exprParser
        |. P.spaces
        |. P.symbol ")"


-- * expr


type Expr
    = LBool Bool
    | LInt Int
    | LString String
    | Var String
    | Get String
    | Eq Expr Expr
    | Add Expr Expr
    | HttpResponseBodyAsString


exprParser : Parser Expr
exprParser =
    P.succeed identity
        |. P.spaces
        |= P.oneOf
           [ binOpParser
           , lBoolParser
           , lIntParser
           , lStringParser
           , varParser
           , getParser
           , httpResponseBodyAsStringParser
           ]
        |. P.spaces

lIntParser : Parser Expr
lIntParser =
    P.succeed LInt
        |= P.int

lBoolParser : Parser Expr
lBoolParser =
    P.succeed LBool
        |= P.oneOf [ P.map (always True) (P.keyword "true")
                   , P.map (always False) (P.keyword "false")
                   ]
lStringParser : Parser Expr
lStringParser =
    P.succeed LString
        |= doubleQuoteString

varParser : Parser Expr
varParser =
    P.map Var variableNameParser

httpResponseBodyAsStringParser : Parser Expr
httpResponseBodyAsStringParser =
    P.succeed HttpResponseBodyAsString
        |. P.keyword "httpResponseBodyAsString"

getParser : Parser Expr
getParser =
    P.succeed Get
        |. P.keyword "get"
        |. P.spaces
        |. P.symbol "("
        |. P.spaces
        |= doubleQuoteString
        |. P.spaces
        |. P.symbol ")"


-- ** variable


variableNameParser : Parser String
variableNameParser =
    P.variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = reserved
        }

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



-- ** bin operator


binOpParser : Parser Expr
binOpParser =
    let
        operators : List (List (P.Operator Expr))
        operators =
            [ [ P.infixOperator (\a b -> Eq a b) (P.symbol "==") P.AssocLeft
              , P.infixOperator (\a b -> Add a b) (P.symbol "+") P.AssocLeft
              ]
            ]

        operandParser : Parser Expr
        operandParser =
            P.oneOf
                [ lBoolParser
                , lIntParser
                , lStringParser
                , varParser
                , getParser
                , httpResponseBodyAsStringParser
                ]
    in
        P.buildExpressionParser operators <|
            P.lazy <| \_ ->
                P.succeed identity
                    |. P.spaces
                    |= P.lazy (\_ -> operandParser)
                    |. P.spaces
