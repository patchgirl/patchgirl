module RequestRunner.App exposing (..)

import Http as Http

import RequestRunner.Message exposing (..)
import RequestRunner.Model exposing (..)
import RequestRunner.Util exposing (..)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Run envs vars ->
        {-
        let
            request = buildRequest <| buildRequestInput envs vars builder
            cmdRequest =
                { method = request.method
                , headers = request.headers
                , url = request.url
                , body = request.body
                , expect = Http.expectString GetResponse
                , timeout = Nothing
                , tracker = Nothing
                }
        in
            (model, Http.request cmdRequest)
            -}
        (model, Cmd.none)

    GetResponse _ ->
      (model, Cmd.none)
