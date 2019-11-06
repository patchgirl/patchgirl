module Icon exposing (..)

import Element exposing (..)
import Html as Html
import Html.Attributes as Html

icon whichIcon =
    html <|
        Html.span []
            [ Html.i
                  [ Html.class "material-icons"
                  , Html.style "vertical-align" "middle"
                  ]
                  [ Html.text whichIcon ]
            ]

iconWithText iconText someText =
    Html.span []
        [ Html.i
              [ Html.class "material-icons"
              , Html.style "vertical-align" "middle"
              ]
              [ Html.text iconText ]
        , Html.text someText
        ]

createFolderIcon =
    icon "create_new_folder"

createFileIcon =
    icon "note_add"

editIcon =
    icon "edit"

deleteIcon =
    icon "delete"
