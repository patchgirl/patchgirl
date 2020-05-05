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
           [ lBoolParser
           , lIntParser
           , httpResponseBodyAsStringParser
           , getParser
           , lStringParser
           , varParser
--           , binOpParser
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
    let
        charsHelp : List Char -> Parser (Step (List Char) String)
        charsHelp revLetters =
            P.oneOf
                [ letterParser |> P.map (\letter -> P.Loop (letter :: revLetters))
                , P.succeed () |> P.map (\_ -> P.Done (String.fromList <| List.reverse revLetters))
                ]
    in
    downcasedLetterParser
        |> P.andThen (\letter -> (P.loop [letter] charsHelp))
        |> P.andThen (\variable ->
                          case Set.member variable reserved of
                              True -> P.problem "reserved word"
                              False -> P.succeed variable
                     )

downcasedLetterParser : Parser Char
downcasedLetterParser =
    P.oneOf
        [ P.map (always 'a') (P.symbol "a")
        , P.map (always 'b') (P.symbol "b")
        , P.map (always 'c') (P.symbol "c")
        , P.map (always 'd') (P.symbol "d")
        , P.map (always 'e') (P.symbol "e")
        , P.map (always 'f') (P.symbol "f")
        , P.map (always 'g') (P.symbol "g")
        , P.map (always 'g') (P.symbol "g")
        , P.map (always 'h') (P.symbol "h")
        , P.map (always 'i') (P.symbol "i")
        , P.map (always 'j') (P.symbol "j")
        , P.map (always 'k') (P.symbol "k")
        , P.map (always 'l') (P.symbol "l")
        , P.map (always 'm') (P.symbol "m")
        , P.map (always 'n') (P.symbol "n")
        , P.map (always 'o') (P.symbol "o")
        , P.map (always 'p') (P.symbol "p")
        , P.map (always 'q') (P.symbol "q")
        , P.map (always 'r') (P.symbol "r")
        , P.map (always 's') (P.symbol "s")
        , P.map (always 't') (P.symbol "t")
        , P.map (always 'u') (P.symbol "u")
        , P.map (always 'v') (P.symbol "v")
        , P.map (always 'w') (P.symbol "w")
        , P.map (always 'x') (P.symbol "x")
        , P.map (always 'y') (P.symbol "y")
        , P.map (always 'z') (P.symbol "z")
        ]

letterParser : Parser Char
letterParser =
    P.oneOf
        [ P.map (always 'A') (P.symbol "A")
        , P.map (always 'B') (P.symbol "B")
        , P.map (always 'C') (P.symbol "C")
        , P.map (always 'D') (P.symbol "D")
        , P.map (always 'E') (P.symbol "E")
        , P.map (always 'F') (P.symbol "F")
        , P.map (always 'G') (P.symbol "G")
        , P.map (always 'G') (P.symbol "G")
        , P.map (always 'H') (P.symbol "H")
        , P.map (always 'I') (P.symbol "I")
        , P.map (always 'J') (P.symbol "J")
        , P.map (always 'K') (P.symbol "K")
        , P.map (always 'L') (P.symbol "L")
        , P.map (always 'M') (P.symbol "M")
        , P.map (always 'N') (P.symbol "N")
        , P.map (always 'O') (P.symbol "O")
        , P.map (always 'P') (P.symbol "P")
        , P.map (always 'Q') (P.symbol "Q")
        , P.map (always 'R') (P.symbol "R")
        , P.map (always 'S') (P.symbol "S")
        , P.map (always 'T') (P.symbol "T")
        , P.map (always 'U') (P.symbol "U")
        , P.map (always 'V') (P.symbol "V")
        , P.map (always 'W') (P.symbol "W")
        , P.map (always 'X') (P.symbol "X")
        , P.map (always 'Y') (P.symbol "Y")
        , P.map (always 'Z') (P.symbol "Z")
        , P.map (always 'a') (P.symbol "a")
        , P.map (always 'b') (P.symbol "b")
        , P.map (always 'c') (P.symbol "c")
        , P.map (always 'd') (P.symbol "d")
        , P.map (always 'e') (P.symbol "e")
        , P.map (always 'f') (P.symbol "f")
        , P.map (always 'g') (P.symbol "g")
        , P.map (always 'g') (P.symbol "g")
        , P.map (always 'h') (P.symbol "h")
        , P.map (always 'i') (P.symbol "i")
        , P.map (always 'j') (P.symbol "j")
        , P.map (always 'k') (P.symbol "k")
        , P.map (always 'l') (P.symbol "l")
        , P.map (always 'm') (P.symbol "m")
        , P.map (always 'n') (P.symbol "n")
        , P.map (always 'o') (P.symbol "o")
        , P.map (always 'p') (P.symbol "p")
        , P.map (always 'q') (P.symbol "q")
        , P.map (always 'r') (P.symbol "r")
        , P.map (always 's') (P.symbol "s")
        , P.map (always 't') (P.symbol "t")
        , P.map (always 'u') (P.symbol "u")
        , P.map (always 'v') (P.symbol "v")
        , P.map (always 'w') (P.symbol "w")
        , P.map (always 'x') (P.symbol "x")
        , P.map (always 'y') (P.symbol "y")
        , P.map (always 'z') (P.symbol "z")
        ]


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
