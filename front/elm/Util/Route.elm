module Util.Route exposing (..)

type Route
    = Home
    | Settings

href : Route -> String
href route =
    let
        pieces =
            case route of
                Home ->
                    []
                Settings ->
                    [ "settings" ]
    in
        "#/" ++ String.join "/" pieces
