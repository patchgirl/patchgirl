module ViewUtil exposing (..)

import Element exposing (..)

-- * Color

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
