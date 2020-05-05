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
           [ varParser
           , lBoolParser
           , lIntParser
           , httpResponseBodyAsStringParser
           , getParser
           , lStringParser
           , binOpParser
           ]
        |. P.spaces

lIntParser : Parser Expr
lIntParser =
    P.int |> P.map LInt

lBoolParser : Parser Expr
lBoolParser =
    P.succeed LBool
        |= P.oneOf [ P.map (always True) (P.keyword "true")
                   , P.map (always False) (P.keyword "false")
                   ]

lStringParser : Parser Expr
lStringParser =
    doubleQuoteString |> P.map LString

httpResponseBodyAsStringParser : Parser Expr
httpResponseBodyAsStringParser =
    P.keyword "httpResponseBodyAsString" |> P.map (always HttpResponseBodyAsString)

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


varParser : Parser Expr
varParser =
    variableNameParser |> P.map Var

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
        onlySpaces : P.Parser ()
        onlySpaces =
            P.chompWhile (\c -> c == ' ')

        lineHelper : List Proc -> Parser (Step TangoAst TangoAst)
        lineHelper revStmts =
            P.succeed identity
                |. P.spaces
                |= P.oneOf
                   [ P.succeed (\stmt -> P.Loop (stmt :: revStmts))
                       |= procParser
                       |. P.spaces
                       |. P.symbol ";"
                       |. onlySpaces
                       |. P.oneOf [ P.symbol "\n", P.end ]
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


-- * show


astAsString : TangoAst -> String
astAsString ast =
    List.map showProc ast |> String.join ("\n")

showProc : Proc -> String
showProc proc =
    case proc of
        AssertEqual expr1 expr2 ->
            "(== " ++ showExpr expr1 ++ showExpr expr2 ++ ")"

        Let str expr ->
            "(Let " ++ str ++ " " ++ showExpr expr ++ ")"

        Set str expr ->
            "(Set " ++ str ++ " " ++ showExpr expr ++ ")"

showExpr : Expr -> String
showExpr expr =
    case expr of
        LBool x ->
            let
                boolAsString =
                    case x of
                        True -> "true"
                        False -> "false"
            in
            "(LBool " ++ boolAsString ++ ")"

        LInt x ->
            "(LInt " ++ String.fromInt(x) ++ ")"

        LString x ->
            "(LString " ++ x ++ ")"

        Var x ->
            "(Var " ++ x ++ ")"

        Get x ->
            "(Get " ++ x ++ ")"

        Eq expr1 expr2 ->
            "(Eq " ++ showExpr expr1 ++ showExpr expr2 ++ ")"

        Add expr1 expr2 ->
            "(Add " ++ showExpr expr1 ++ showExpr expr2 ++ ")"

        HttpResponseBodyAsString ->
            "(HttpResponseBodyAsString)"
