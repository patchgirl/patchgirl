module Label exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Events as Events
import Element.Input as Input

import Html as Html
import Html.Attributes as Html
import ViewUtil exposing (..)

labelAttrs : List(Attribute a)
labelAttrs =
     [ padding 10
     , Border.solid
     , Border.width 1
     , Border.rounded 5
     , height fill
     ]

labelSuccess : String -> Element a
labelSuccess labelText =
    let
        attributes =
            [ Background.color backgroundGreen
            , Border.color borderGreen
            , Font.color fontGreen
            ]
    in
        el (attributes ++ labelAttrs) (text labelText)

labelWarning : String -> Element a
labelWarning labelText =
    let
        attributes =
            [ Background.color backgroundOrange
            , Border.color borderOrange
            , Font.color fontOrange
            ]
    in
        el (attributes ++ labelAttrs) (text labelText)

labelError : String -> Element a
labelError labelText =
    let
        attributes =
            [ Background.color backgroundRed
            , Border.color borderRed
            , Font.color fontRed
            ]
    in
        el (attributes ++ labelAttrs) (text labelText)

labelInfo : String -> Element a
labelInfo = labelSuccess
