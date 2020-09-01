module Interpolator exposing (..)

import Element exposing (..)
import Element.Font as Font
import Application.Type exposing(..)
import Dict
import Util exposing (..)


type InterpolatedString
    = RawString String
    | MissingKey String
    | InterpolatedKey String String

interpolate : List (Storable NewKeyValue KeyValue) -> StringTemplate -> List InterpolatedString
interpolate envKeyValues stringTemplate =
    let
        interpolateSingle : Template -> InterpolatedString
        interpolateSingle template =
            case template of
                Sentence str ->
                    RawString str

                Key str ->
                    let
                        keyValues =
                            envKeyValues
                                |> List.map latestValueOfStorable
                                |> Dict.fromList
                    in
                    case Dict.get str keyValues of
                        Nothing ->
                            MissingKey str

                        Just value ->
                            InterpolatedKey str (templatedStringAsString value)
    in
    List.map interpolateSingle stringTemplate


allInterpolatedStringAsElement : List InterpolatedString -> Element a
allInterpolatedStringAsElement interpolatedStrings =
    let
        showInterpolatedStringSingle : InterpolatedString -> Element a
        showInterpolatedStringSingle interpolatedString =
            case interpolatedString of
                RawString str -> text str
                MissingKey str -> el [ Font.color redColor, Font.bold ] <| text ("{{" ++ str ++ "}}")
                InterpolatedKey _ str -> el [ Font.color greenColor, Font.bold ] (text str)
    in
    row [ centerY, centerX, Font.center, Font.family
              [ Font.typeface "Roboto mono" ]
        ] <| List.map showInterpolatedStringSingle interpolatedStrings


onlyInterpolatedStringAsElement : List InterpolatedString -> Element a
onlyInterpolatedStringAsElement interpolatedStrings =
    let
        showInterpolatedStringSingle : InterpolatedString -> Element a
        showInterpolatedStringSingle interpolatedString =
            case interpolatedString of
                RawString _ -> none
                MissingKey str -> el [ Font.color redColor, Font.bold ] <| text ("{{" ++ str ++ "}}")
                InterpolatedKey key value ->
                    row []
                        [ text (key ++ ": ")
                        , el [ Font.color greenColor, Font.bold ] (text value)
                        ]
    in
    column [ spacing 10, centerX, Font.center, Font.family
              [ Font.typeface "Roboto mono" ]
        ] <| List.map showInterpolatedStringSingle interpolatedStrings
