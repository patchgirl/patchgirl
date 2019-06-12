module Curl.Util exposing (..)

import RequestRunner.Model as RequestRunner
import Util.List as List
import RequestInput.Model as RequestInput

showRequestAsCurl : RequestInput.Model -> String
showRequestAsCurl request =
    let
        headers : List String
        headers = List.map (\(h, v) -> "-H '" ++ h ++ ":" ++ v ++ "'") request.headers
        headersForCurl : String
        headersForCurl = List.join " " headers
    in
        "curl '" ++ request.url ++ "'" ++ headersForCurl
