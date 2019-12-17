module ViewUtil exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Events as Events
import Element.Input as Input

import Html as Html
import Html.Events as Html
import Html.Attributes as Html
import Json.Decode as Json

-- * color

black : Color
black =
    rgb255 0 0 0

primaryColor : Color
primaryColor =
    rgb255 220 118 118

secondaryColor : Color
secondaryColor =
    rgb255 249 227 208

fontGreen : Color
fontGreen =
    rgb255 21 87 36

backgroundGreen : Color
backgroundGreen =
    rgb255 212 237 218

borderGreen : Color
borderGreen =
    rgb255 195 230 203

fontRed : Color
fontRed =
    rgb255 114 28 36

backgroundRed : Color
backgroundRed =
    rgb255 248 215 218

borderRed : Color
borderRed =
    rgb255 245 198 203

fontOrange : Color
fontOrange =
    rgb255 133 100 4

backgroundOrange : Color
backgroundOrange =
    rgb255 255 243 205

borderOrange : Color
borderOrange =
    rgb255 255 238 186

colorToString : Color -> String
colorToString color =
    let
        { red, green, blue, alpha } = toRgb color
        rgbFloatToString : Float -> String
        rgbFloatToString rgbFloat = String.fromFloat(rgbFloat * 255)
    in
        "rgb(" ++ rgbFloatToString(red) ++ "," ++ rgbFloatToString(green) ++ "," ++ rgbFloatToString(blue) ++ ")"

-- * icon

icon : String -> Element a
icon whichIcon =
    iconWithText whichIcon ""

iconWithTextAndColor : String -> String -> Color -> Element a
iconWithTextAndColor iconText someText color =
    html <|
        Html.span []
            [ Html.i
                  [ Html.class "material-icons"
                  , Html.style "vertical-align" "middle"
                  , Html.style "color" (colorToString color)
                  ]
                  [ Html.text iconText ]
            , Html.text someText
            ]

iconWithText : String -> String -> Element a
iconWithText iconText someText =
    iconWithTextAndColor iconText someText black

createFolderIcon =
    icon "create_new_folder"

createFileIcon =
    icon "note_add"

editIcon =
    icon "edit"

deleteIcon =
    icon "delete"

playIcon =
    icon "play-arrow"

sendIcon =
    icon "send"

addIcon =
    icon "add_circle_outline"

-- * label

labelAttrs : List(Attribute a)
labelAttrs =
     [ padding 10
     , Border.solid
     , Border.width 1
     , Border.rounded 5
     , paddingXY 10 10
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


-- * util

onEnter : a -> Attribute a
onEnter msg =
  let
    isEnter code =
      if code == 13 then
        Json.succeed msg
      else
        Json.fail "not ENTER"
    in
      Html.on "keydown" (Json.andThen isEnter Html.keyCode) |> htmlAttribute
