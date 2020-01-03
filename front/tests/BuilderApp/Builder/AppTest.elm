module BuilderApp.Builder.AppTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import BuilderApp.Builder.App exposing (..)


suite : Test
suite =
    describe "BuilderApp.Builder.App module"
        [ describe "when building request"
            [ test "remove http from url" <|
                \_ ->
                  Expect.equal (removeSchemeFromUrl "http://foo.com") "foo.com"

            , test "remove https from url" <|
                \_ ->
                  Expect.equal (removeSchemeFromUrl "https://foo.com") "foo.com"

            , test "don't remove http when url is malformed" <|
                \_ ->
                  Expect.equal (removeSchemeFromUrl "http:/ /foo.com") "http:/ /foo.com"

            , test "don't touch when url doesn't have a scheme" <|
                \_ ->
                  Expect.equal (removeSchemeFromUrl "foo.com") "foo.com"
            ]
        ]
