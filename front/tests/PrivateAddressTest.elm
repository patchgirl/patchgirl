module PrivateAddressTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import PrivateAddress exposing (..)


suite : Test
suite =
    describe "PrivateAddress module"
        [ describe "can match local address"
            [ test "works with 'localhost'" <|
                \_ ->
                    let
                        input =
                            "localhost"
                    in
                        Expect.equal (isPrivateAddress input) True

            , test "works with class A network" <|
                \_ ->
                    let
                        inputs =
                            [ "10.0.0.1"
                            , "10.255.255.255"
                            ]
                    in
                        Expect.equal (List.all isPrivateAddress inputs) True

            , test "works with class B network" <|
                \_ ->
                    let
                        inputs =
                            [ "172.16.0.1"
                            , "172.31.255.255"
                            ]
                    in
                        Expect.equal (List.all isPrivateAddress inputs) True

            , test "works with class C network" <|
                \_ ->
                    let
                        inputs =
                            [ "192.168.0.1"
                            , "192.168.255.255"
                            ]
                    in
                        Expect.equal (List.all isPrivateAddress inputs) True

            , test "works localhost network" <|
                \_ ->
                    let
                        inputs =
                            [ "127.0.0.1"
                            , "127.255.255.255"
                            ]
                    in
                        Expect.equal (List.all isPrivateAddress inputs) True

            , test "fails with public ips" <|
                \_ ->
                    let
                        inputs =
                            [ "9.255.255.255"
                            , "11.0.0.1"
                            , "172.15.0.1"
                            , "127.255.255.255"
                            , "192.167.0.1"
                            , "192.169.0.1"
                            , "128.0.0.1"
                            , "whatever"
                            ]
                    in
                        Expect.equal (List.all (isPrivateAddress >> not) inputs) False
            ]
        ]
