module Util exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html as Html
import Html.Attributes as Html
import Html.Events as Html
import Json.Decode as Json



-- * color


boxShadow : Attr a b
boxShadow =
    Border.shadow
        { offset = ( 0, 1 )
        , size = 0
        , blur = 1
        , color = rgba255 10 22 70 0.1
        }


white : Color
white =
    rgb255 255 255 255


lightGrey : Color
lightGrey =
    rgb255 245 248 250


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
        { red, green, blue } =
            toRgb color

        rgbFloatToString : Float -> String
        rgbFloatToString rgbFloat =
            String.fromFloat (rgbFloat * 255)
    in
    "rgb(" ++ rgbFloatToString red ++ "," ++ rgbFloatToString green ++ "," ++ rgbFloatToString blue ++ ")"



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


iconWithTextAndColorAndAttr : String -> String -> Color -> List (Html.Attribute a) -> Element a
iconWithTextAndColorAndAttr iconText someText color someAttr =
    html <|
        Html.span []
            [ Html.i
                ([ Html.class "material-icons"
                 , Html.style "vertical-align" "middle"
                 , Html.style "color" (colorToString color)
                 ]
                    ++ someAttr
                )
                [ Html.text iconText ]
            , Html.text someText
            ]


iconWithText : String -> String -> Element a
iconWithText iconText someText =
    iconWithTextAndColor iconText someText black


createFolderIcon : Element a
createFolderIcon =
    icon "create_new_folder"


createFileIcon : Element a
createFileIcon =
    icon "note_add"


editIcon : Element a
editIcon =
    icon "edit"


deleteIcon : Element a
deleteIcon =
    icon "delete"


playIcon : Element a
playIcon =
    icon "play-arrow"


sendIcon : Element a
sendIcon =
    icon "send"


addIcon : Element a
addIcon =
    icon "add_circle_outline"


clearIcon : Element a
clearIcon =
    icon "clear"


arrowDownwardIcon : Element a
arrowDownwardIcon =
    icon "arrow_downward"



-- * label


labelAttrs : List (Attribute a)
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


-- * border


borderSuccess : Attribute a
borderSuccess =
    Border.color borderGreen

borderWarning : Attribute a
borderWarning =
    Border.color borderOrange

borderError : Attribute a
borderError =
    Border.color borderRed


-- * background


backgroundSuccess : Attribute a
backgroundSuccess =
    Background.color backgroundGreen

backgroundWarning : Attribute a
backgroundWarning =
    Background.color backgroundOrange

backgroundError : Attribute a
backgroundError =
    Background.color backgroundRed


-- * onEnter


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


onEnter2 : a -> Attribute a
onEnter2 msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg

            else
                Json.fail "not ENTER"
    in
    Html.on "keydown" (Json.andThen isEnter Html.keyCode) |> htmlAttribute


onEnterWithInput : (String -> a) -> Attribute a
onEnterWithInput tagger =
    let
        isEnter code =
            if code == 13 then
                Json.succeed ""

            else
                Json.fail ""

        decodeEnter =
            Json.andThen isEnter Html.keyCode
    in
    htmlAttribute <|
        Html.on "keydown" <|
            Json.map2 (\_ value -> tagger value) decodeEnter Html.targetValue



-- * maybe


maybeExists : Maybe a -> (a -> Bool) -> Bool
maybeExists m f =
    case m of
        Just a ->
            f a

        Nothing ->
            False


catMaybes : List (Maybe a) -> List a
catMaybes =
    List.filterMap identity



-- * list


addToListAfterPredicate : List a -> (a -> Bool) -> a -> List a
addToListAfterPredicate list predicate newElem =
    case list of
        x :: xs ->
            case predicate x of
                False ->
                    x :: addToListAfterPredicate xs predicate newElem

                True ->
                    x :: newElem :: xs

        [] ->
            []
