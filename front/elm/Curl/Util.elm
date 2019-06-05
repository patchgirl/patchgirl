module Curl.Util exposing (..)

import RequestRunner.Model as RequestRunner

showRequestAsCurl : RequestRunner.Request -> String
showRequestAsCurl request =
    "curl '" ++ request.url ++ "'"
