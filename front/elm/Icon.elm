module Icon exposing (..)

import Element exposing (..)
import Html as Html
import Html.Attributes as Html
import ViewUtil exposing (..)

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
