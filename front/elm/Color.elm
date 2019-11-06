module Color exposing (..)

import Element exposing (..)

black : Color
black = rgb255 0 0 0

primaryColor : Color
primaryColor = rgb255 220 118 118

secondaryColor : Color
secondaryColor = rgb255 249 227 208

colorToString : Color -> String
colorToString color =
    let
        { red, green, blue, alpha } = toRgb color
        rgbFloatToString : Float -> String
        rgbFloatToString rgbFloat = String.fromFloat(rgbFloat * 255)
    in
        "rgb(" ++ rgbFloatToString(red) ++ "," ++ rgbFloatToString(green) ++ "," ++ rgbFloatToString(blue) ++ ")"
