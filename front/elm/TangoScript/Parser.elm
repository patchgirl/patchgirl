module TangoScript.Parser exposing ( parseTangoscript
                                   , showErrors
                                   , tangoParser
                                   , exprParser
                                   , procParser
                                   , astAsString
                                   )

import Parser as P exposing((|.), (|=), Step, Parser)
import Parser.Expression as P exposing (OperatorTable)
import Set exposing (Set)
import TangoScript.DoubleQuoteString exposing(doubleQuoteString)
import Application.Type exposing(..)


-- * parse


parseTangoscript : String -> Result (List P.DeadEnd) TangoAst
parseTangoscript =
    P.run tangoParser


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


exprParser : Parser Expr
exprParser =
    P.succeed identity
        |. P.spaces
        |= P.oneOf
           [ lBoolParser
           , lIntParser
           , httpResponseBodyAsStringParser
           , httpResponseStatusParser
           , listParser
           , varParser
           , getParser
           , lStringParser
           , binOpParser
           ]
        |. P.spaces

lIntParser : Parser Expr
lIntParser =
    P.int |> P.map (\int -> (EPrim (PInt int)))

lBoolParser : Parser Expr
lBoolParser =
    P.succeed EPrim
        |= P.oneOf [ P.map (always (PBool True)) (P.keyword "true")
                   , P.map (always (PBool False)) (P.keyword "false")
                   ]

lStringParser : Parser Expr
lStringParser =
    doubleQuoteString |> P.map LString

httpResponseBodyAsStringParser : Parser Expr
httpResponseBodyAsStringParser =
    P.keyword "httpResponseBodyAsString" |> P.map (always HttpResponseBodyAsString)

httpResponseStatusParser : Parser Expr
httpResponseStatusParser =
    P.keyword "httpResponseStatus" |> P.map (always HttpResponseStatus)

getParser : Parser Expr
getParser =
    P.succeed Fetch
        |. P.keyword "get"
        |. P.spaces
        |. P.symbol "("
        |. P.spaces
        |= doubleQuoteString
        |. P.spaces
        |. P.symbol ")"

-- ** list


listParser : Parser Expr
listParser =
    P.sequence
        { start = "["
        , separator = ","
        , end = "]"
        , spaces = P.spaces
        , item = P.lazy (\_ -> exprParser)
        , trailing = P.Forbidden
        }
        |> P.map EList
        |> P.andThen eListGetAtParser


-- ** list - get at index


eListGetAtParser : Expr -> Parser Expr
eListGetAtParser expr =
    let
        listIndex : Parser Expr
        listIndex =
            P.succeed identity
                |. P.symbol "["
                |. P.spaces
                |= P.lazy (\_ -> exprParser)
                |. P.spaces
                |. P.symbol "]"

        maybeListIndex : P.Parser (Maybe Expr)
        maybeListIndex =
            P.oneOf [ listIndex |> P.map Just
                    , P.succeed Nothing
                    ]

        variableOrListGetAt : Expr -> Maybe Expr -> Expr
        variableOrListGetAt var mExpr =
            case mExpr of
                Just e -> EAccess var e
                Nothing -> var
    in
    maybeListIndex |> P.map (variableOrListGetAt expr)


-- ** variable


varParser : Parser Expr
varParser =
    variableNameParser
        |> P.map Var
        |> P.andThen eListGetAtParser


variableNameParser : Parser String
variableNameParser =
  P.variable
    { start = Char.isLower
    , inner = \c -> Char.isAlphaNum c || c == '_'
    , reserved = reserved
    }


-- * parser


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

        Fetch x ->
            "(Fetch " ++ x ++ ")"

        Eq expr1 expr2 ->
            "(Eq " ++ showExpr expr1 ++ showExpr expr2 ++ ")"

        Add expr1 expr2 ->
            "(Add " ++ showExpr expr1 ++ showExpr expr2 ++ ")"

        HttpResponseBodyAsString ->
            "(HttpResponseBodyAsString)"

        HttpResponseStatus ->
            "(HttpResponseStatus)"

        EPrim (PBool bool) ->
            let
                boolAsString =
                    case bool of
                        True -> "true"
                        False -> "false"
            in
            "(PBool " ++ boolAsString ++ " )"

        EPrim (PInt x) ->
            "(PInt " ++ String.fromInt(x) ++ ")"

        EList _ ->
            "(EList)"

        EAccess _ _ ->
            "(EAccess)"

-- ** error


showErrors : List P.DeadEnd -> String
showErrors deadEnds =
    List.map showError deadEnds |> List.take 1 |> String.join "\n"

showError : P.DeadEnd -> String
showError deadEnd =
    let
        error =
            case deadEnd.problem of
                P.Expecting s -> "Expecting"
                P.ExpectingInt -> "Expecting an integer"
                P.ExpectingHex -> "Expecting an hexadecimal"
                P.ExpectingOctal -> "Expecting an octal"
                P.ExpectingBinary -> "Expecting a binary"
                P.ExpectingFloat -> "Expecting a float"
                P.ExpectingNumber -> "Expecting a number"
                P.ExpectingVariable -> "Expecting a variable"
                P.ExpectingSymbol s -> "Expecting symbol [" ++ s ++ "]"
                P.ExpectingKeyword s -> "Expecting keyword [" ++ s ++ "]"
                P.ExpectingEnd -> "Expecting end"
                P.UnexpectedChar -> "Unexpected char"
                P.Problem s -> "Problem with string [" ++ s ++ "]"
                P.BadRepeat -> "Bad repeat"
    in
    error ++ " at line " ++ String.fromInt(deadEnd.row)
