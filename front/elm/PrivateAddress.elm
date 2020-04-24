module PrivateAddress exposing (..)

import IP as IP
import Regex as Regex
import Subnet as Subnet


isPrivateAddress : String -> Bool
isPrivateAddress url =
    let
        isIP =
            IP.validate url

        classANetwork =
            ( "10.0.0.0", "255.0.0.0" )

        classBNetwork =
            ( "172.16.0.0", "255.240.0.0" )

        classCNetwork =
            ( "192.168.0.0", "255.255.0.0" )

        localhostNetwork =
            ( "127.0.0.0", "255.0.0.0" )

        isPrivateIP =
            Subnet.included classANetwork url
                || Subnet.included classBNetwork url
                || Subnet.included classCNetwork url
                || Subnet.included localhostNetwork url

        isLocalhost =
            let
                localhostRegex : Regex.Regex
                localhostRegex =
                    Maybe.withDefault Regex.never <|
                        Regex.fromStringWith
                            { caseInsensitive = True
                            , multiline = False
                            }
                            "^localhost(:|/|$)"
            in
            Regex.contains localhostRegex url
    in
    (isIP && isPrivateIP) || isLocalhost
