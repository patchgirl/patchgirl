module TangoScript.Parser exposing (..)

import Parser as P exposing((|.), (|=), Step, Parser)
import Parser.Expression as P exposing (OperatorTable)
import Set exposing (Set)


-- * AST


type Expr
  = Lit Lit
  | Op Binop Expr Expr
  | AssertEqual Expr Expr
  | Let String Expr
  | Var String

type Lit
  = LInt Int
  | LBool Bool

type Binop
    = Add
    | Sub


-- * reserved keywords


reserved : Set String
reserved =
    Set.fromList
        [ "assertEqual"
        , "let"
        , "true"
        , "false"
        ]


-- * parser


-- ** expr


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


-- ** litteral


litParser : Parser Expr
litParser =
    let
        lIntParser =
            P.succeed (Lit << LInt)
                |. P.spaces
                |= P.int
                |. P.spaces

        lBoolParser =
            P.succeed (Lit << LBool)
                |. P.spaces
                |= P.oneOf [ P.map (always True) (P.symbol "true")
                           , P.map (always False) (P.symbol "false")
                           ]
                |. P.spaces
    in
    P.oneOf [ lIntParser, lBoolParser ]


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


-- ** let


letParser : Parser Expr
letParser =
    P.succeed Let
        |. P.keyword "let" |. P.spaces

        |= variableNameParser |. P.spaces

        |. P.symbol "=" |. P.spaces

        |= (P.lazy <| \_ -> exprParser)


-- ** bin operator


binOpParser : Parser Expr
binOpParser =
    let
        operators : List (List (P.Operator Expr))
        operators =
            [ [ P.infixOperator (\a b -> Op Add a b) (P.symbol "+") P.AssocLeft
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

-- ** assert equal


assertEqualParser : Parser Expr
assertEqualParser =
    P.succeed AssertEqual
        |. P.keyword "assertEqual" |. P.spaces
        |. P.symbol "(" |. P.spaces
        |= (P.lazy <| \_ -> exprParser) |. P.spaces
        |. P.symbol "," |. P.spaces
        |= (P.lazy <| \_ -> exprParser) |. P.spaces
        |. P.symbol ")"
