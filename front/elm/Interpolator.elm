module Interpolator exposing (..)

import Element exposing (..)
import Element.Font as Font
import Application.Type exposing(..)
import Dict
import Util exposing (..)
import StringTemplate exposing(..)
import Html.Attributes as Html


-- * model


type InterpolatedString
    = RawString String
    | MissingKey String
    | InterpolatedKey String String Bool


-- * interpolate


interpolate : List KeyValue -> StringTemplate -> List InterpolatedString
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
                                |> List.map (\r -> (editedOrNotEditedValue r.key, (editedOrNotEditedValue r.value, editedOrNotEditedValue r.hidden)))
                                |> Dict.fromList
                    in
                    case Dict.get str keyValues of
                        Nothing ->
                            MissingKey str

                        Just (value, hidden) ->
                            InterpolatedKey str (templatedStringAsString value) hidden
    in
    List.map interpolateSingle stringTemplate


-- * util


{-
  input: "coucou{{host}} ?"
  output: coucou <b>api.com</b> ?
-}
allInterpolatedStringAsElement : List InterpolatedString -> Element a
allInterpolatedStringAsElement interpolatedStrings =
    let
        showInterpolatedStringSingle : InterpolatedString -> Element a
        showInterpolatedStringSingle interpolatedString =
            case interpolatedString of
                RawString str -> text str
                MissingKey str -> el [ Font.color redColor, Font.bold ] <| text ("{{" ++ str ++ "}}")
                InterpolatedKey _ str hidden ->
                    case hidden of
                        False ->
                            el [ Font.color greenColor, Font.bold ] (text str)

                        True ->
                            let
                                hiddenValue =
                                    String.repeat (String.length str) "*"
                            in
                            el [ Font.color greenColor, Font.bold ] (text hiddenValue)
    in
    row [ centerY, centerX, Font.center, Font.family
              [ Font.typeface "Roboto mono" ]
        ] <| List.map showInterpolatedStringSingle interpolatedStrings



{-
  input: "hello {{firstname}} {{lastname}}!"
  output: |
    firstname: John
    lastname: Doe
-}
onlyInterpolatedStringAsElement : List InterpolatedString -> Element a
onlyInterpolatedStringAsElement interpolatedStrings =
    let
        showInterpolatedStringSingle : InterpolatedString -> Element a
        showInterpolatedStringSingle interpolatedString =
            case interpolatedString of
                RawString _ -> none
                MissingKey str -> el [ Font.color redColor, Font.bold ] <| text ("{{" ++ str ++ "}}")
                InterpolatedKey key value hidden ->
                    row []
                        [ text (key ++ ": ")
                        , case hidden of
                              False ->
                                  el [ Font.color greenColor, Font.bold ] (text value)

                              True ->
                                  let
                                      hiddenValue =
                                          String.repeat (String.length value) "*"
                                  in
                                  el [ Font.color greenColor, Font.bold ] (text hiddenValue)
                        ]
    in
    column [ spacing 10, centerX, Font.center, Font.family
              [ Font.typeface "Roboto mono" ]
           ] <| List.map showInterpolatedStringSingle interpolatedStrings


-- * display wrapper


showFullInterpolation : String -> List KeyValue -> String -> Element a
showFullInterpolation class keyValues str =
    let
        template =
            stringToTemplate str

        interpolated =
            interpolate keyValues template

        value =
            allInterpolatedStringAsElement interpolated
    in
    case stringTemplateContainsKey template of
        False -> none
        True ->
            el [ centerY
               , padding 10
               , htmlAttribute (Html.class class)
               ] value

showOnlyInterpolation : List KeyValue -> String -> Element a
showOnlyInterpolation keyValues str =
    let
        template =
            stringToTemplate str

        interpolated =
            interpolate keyValues template

        value =
            onlyInterpolatedStringAsElement interpolated
    in
    case stringTemplateContainsKey template of
        False -> none
        True ->
            el [ centerY
               , padding 10
               , htmlAttribute (Html.class "left-arrow-box")
               ] value
