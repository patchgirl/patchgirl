module Banner exposing (..)

import Element exposing (..)
import Element.Font as Font
import Element.Background as Background
import Page exposing (..)
import Util exposing(..)

enableRunnerBanner : Element a
enableRunnerBanner =
    row [ paddingXY 20 10
        , centerY, centerX
        , Background.color secondaryColor
        , Font.color primaryColor
        , Font.center
        ]
    [ el [ centerX ] (text "Offline mode - Run the ")
    , link [ centerX ] { label = el [ Font.underline ] (text "Patchgirl Runner App")
                       , url = href (DocumentationPage PatchGirlRunnerAppDoc)
                       }
    , el [ centerX ] (text " to have the best experience!")
    ]
