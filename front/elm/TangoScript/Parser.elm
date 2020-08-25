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
import Dict exposing (Dict)


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
        , "httpResponseStatus"
        , "true"
        , "false"
        ]


-- * proc

{- a procedure is an action to be executed.
  It usually performs a side effect (setting a variable in a certain scope for exemple)
  so you can view it as a function of type: executeProc :: IO ()
  A list of procs can constitue a tangoscript program
-}
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


-- ** set

{-
  set procedure is meant to set variable with a global scope
  e.g: set("a", 1);
-}
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


-- ** let

{-
  `let` procedure is meant to set variables with a local scope
  e.g: var a = 1;
-}
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


{-
  assertEqual is a function that compares two expressions
  eg: assertEqual(1, 2);
-}
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


{- expression is a self recursive type
  it cannot appear at the top level of a tangoscript program (only procedure can)
  so the following program is invalid: `someVar[0];`
  whereAs this is valid: `var a = someVar[0];`
 -}
exprParser : Parser Expr
exprParser =
    P.succeed identity
        |. P.spaces
        |= P.oneOf
           [ lBoolParser
           , httpResponseBodyAsStringParser
           , httpResponseStatusParser
           , pgSimpleTableParser
           , pgRichTableParser
           , listParser
           , varParser
           , lIntParser
           , getParser
           , lStringParser
           , eJsonParser
           , binOpParser
           ]
        |. P.spaces


-- ** not accessible expr

{- these expressions cannot be promoted to an accessible operator (e.g: `[]`)
   because this doesn't make sense:
     - 1[0];
     - true[0];
-}
lIntParser : Parser Expr
lIntParser =
    P.int |> P.map LInt

lBoolParser : Parser Expr
lBoolParser =
    P.oneOf [ P.map (always (LBool True)) (P.keyword "true")
            , P.map (always (LBool False)) (P.keyword "false")
            ]


-- ** accessible expr


{- these expressions can be promoted to an accessible operator
   because this make sense:
     - "hello there"[4]
     - [1, 2, 3, 4][0]
     - ...
 -}

lStringParser : Parser Expr
lStringParser =
    doubleQuoteString
        |> P.map LString
        |> P.andThen promoteToAccessOpParser

varParser : Parser Expr
varParser =
    variableNameParser
        |> P.map LVar
        |> P.andThen promoteToAccessOpParser

variableNameParser : Parser String
variableNameParser =
  P.variable
    { start = Char.isLower
    , inner = \c -> Char.isAlphaNum c || c == '_'
    , reserved = reserved
    }

httpResponseBodyAsStringParser : Parser Expr
httpResponseBodyAsStringParser =
    P.keyword "httpResponseBodyAsString"
        |> P.map (always LHttpResponseBodyAsString)
        |> P.andThen promoteToAccessOpParser

httpResponseStatusParser : Parser Expr
httpResponseStatusParser =
    P.keyword "httpResponseStatus"
        |> P.map (always LHttpResponseStatus)
        |> P.andThen promoteToAccessOpParser

pgSimpleTableParser : Parser Expr
pgSimpleTableParser =
    P.keyword "postgresResponseAsArray"
        |> P.map (always LPgSimpleResponse)
        |> P.andThen promoteToAccessOpParser

pgRichTableParser : Parser Expr
pgRichTableParser =
    P.keyword "postgresResponse"
        |> P.map (always LPgRichResponse)
        |> P.andThen promoteToAccessOpParser

getParser : Parser Expr
getParser =
    P.succeed LFetch
        |. P.keyword "get"
        |. P.spaces
        |. P.symbol "("
        |. P.spaces
        |= doubleQuoteString
        |. P.spaces
        |. P.symbol ")"
        |> P.andThen promoteToAccessOpParser

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
        |> P.map LList
        |> P.andThen promoteToAccessOpParser


-- ** access operator


promoteToAccessOpParser : Expr -> Parser Expr
promoteToAccessOpParser expr =
    let
        indexParser : Parser Expr
        indexParser =
            P.succeed identity
                |. P.symbol "["
                |. P.spaces
                |= P.lazy (\_ -> exprParser)
                |. P.spaces
                |. P.symbol "]"

        maybeIndexParser : P.Parser (Maybe Expr)
        maybeIndexParser =
            P.oneOf [ indexParser |> P.map Just
                    , P.succeed Nothing
                    ]

        promote : Expr -> Maybe Expr -> Parser Expr
        promote var mIndex =
            case mIndex of
                Just index ->
                    promoteToAccessOpParser (LAccessOp expr index)
                Nothing ->
                    P.succeed expr

    in
    maybeIndexParser |> P.andThen (promote expr)


-- ** json


eJsonParser : Parser Expr
eJsonParser =
    jsonParser
        |> P.map LJson
        |> P.andThen promoteToAccessOpParser

jsonValueParser : Parser Json
jsonValueParser =
    let
        jIntParser : Parser Json
        jIntParser =
            P.int |> P.map JInt

        jBoolParser : Parser Json
        jBoolParser =
            P.oneOf [ P.map (always (JBool True)) (P.keyword "true")
                    , P.map (always (JBool False)) (P.keyword "false")
                    ]

        jStringParser : Parser Json
        jStringParser =
            doubleQuoteString |> P.map JString

        jFloatParser : Parser Json
        jFloatParser =
            P.float |> P.map JFloat

        jArrayParser : Parser Json
        jArrayParser =
            P.sequence
                { start = "["
                , separator = ","
                , end = "]"
                , spaces = P.spaces
                , item = P.lazy (\_ -> jsonValueParser)
                , trailing = P.Forbidden
                } |> P.map JArray
    in
    P.oneOf [ P.backtrackable jIntParser
            , jFloatParser
            , jBoolParser
            , jStringParser
            , P.lazy (\_ -> jArrayParser)
            , P.lazy (\_ -> jsonParser)
            ]

jsonParser : Parser Json
jsonParser =
    let
        jsonKeyValueParser : Parser (String, Json)
        jsonKeyValueParser =
            P.succeed (\key value -> (key, value))
                |. P.spaces
                |= doubleQuoteString
                |. P.spaces
                |. P.symbol ":"
                |. P.spaces
                |= jsonValueParser
    in
    P.sequence
        { start = "{"
        , separator = ","
        , end = "}"
        , spaces = P.spaces
        , item = jsonKeyValueParser
        , trailing = P.Forbidden
        }
        |> P.map (\keyValues -> JObject (Dict.fromList keyValues))


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
            [ [ P.infixOperator (\a b -> LEq a b) (P.symbol "==") P.AssocLeft
             -- , P.infixOperator (\a b -> Add a b) (P.symbol "+") P.AssocLeft
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

        LVar x ->
            "(LVar " ++ x ++ ")"

        LFetch x ->
            "(LFetch " ++ x ++ ")"

        LEq expr1 expr2 ->
            "(Eq " ++ showExpr expr1 ++ showExpr expr2 ++ ")"

        LHttpResponseBodyAsString ->
            "(LHttpResponseBodyAsString)"

        LHttpResponseStatus ->
            "(LHttpResponseStatus)"

        LList x ->
            List.map showExpr x
                |> String.join ", "
                |> \s ->  "(LList " ++ s ++ " )"

        LAccessOp _ _ ->
            "(LAccessOp)"

        LJson json ->
            "(LJson " ++ showJson json ++ ")"

        LFloat x ->
            "(LFloat " ++ String.fromFloat x ++ ")"

        LNull ->
            "(LNull)"

        LRowElem (key, value) ->
            "(LRowElem " ++ key ++ " " ++ showExpr value ++ ")"

        LPgSimpleResponse ->
            "(LPgSimpleResponse)"

        LPgRichResponse ->
            "(LPgRichResponse)"


showJson : Json -> String
showJson json =
    case json of
        JInt x -> "(JInt " ++ String.fromInt(x) ++ " )"
        JFloat x -> "(JFloat " ++ String.fromFloat(x) ++ " )"
        JBool bool ->
            let
                boolAsString =
                    case bool of
                        True -> "true"
                        False -> "false"
            in
            "(JBool " ++ boolAsString ++ " )"

        JString str -> "(JString " ++ str ++ ")"
        JArray x ->
            List.map showJson x
                |> String.join ", "
                |> \s -> "(JArray " ++ s ++ ")"
        JObject x ->
            Dict.toList x
                |> List.map (\(key, value) -> "(Key " ++ key ++ ", Value " ++ showJson value ++ " )")
                |> String.join ", "





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
