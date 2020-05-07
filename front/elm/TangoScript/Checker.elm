module TangoScript.Checker exposing (showError, check, AstError)

import TangoScript.Parser exposing(..)
import Util exposing(..)
import Application.Type exposing(..)


type AstError
    = IncompatibleType Expr Expr


-- * show


showError : AstError -> String
showError astError =
    case astError of
        IncompatibleType a b ->
            "IncompatibleType: [" ++ typeof a ++ "] " ++ "[" ++ typeof b ++ "]"


-- * check


check : TangoAst -> List AstError
check tangoAst =
    List.map checkProc tangoAst |> catMaybes

checkProc : Proc -> Maybe AstError
checkProc proc =
    case proc of
        AssertEqual expr1 expr2 ->
            checkExpr expr1 expr2

        _ ->
            Nothing

checkExpr : Expr -> Expr -> Maybe AstError
checkExpr expr1 expr2 =
    case compatible expr1 expr2 of
        True ->
            Nothing

        False ->
            Just (IncompatibleType expr1 expr2)


-- * typeof


typeof : Expr -> String
typeof expr =
    case expr of
        LBool _ -> "LBool"
        LInt _ -> "LInt"
        LString _ -> "LString"
        Var _ -> "*"
        Fetch _ -> "*"
        Eq _ _ -> "*"
        Add _ _ -> "*"
        HttpResponseBodyAsString -> "LString"


compatible : Expr -> Expr -> Bool
compatible expr1 expr2 =
    let
        hasWildCard =
            List.member "*" [ typeof expr1, typeof expr2 ]

        areCompatible =
            typeof expr1 == typeof expr2
    in
    hasWildCard || areCompatible
