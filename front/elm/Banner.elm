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

sandboxBanner : String -> Element a
sandboxBanner githubOauthLink =
    el [ paddingXY 20 10
       , centerX, centerY
       , Background.color secondaryColor
       , Font.color primaryColor
       , Font.center
       ] <|
        row [ centerX ]
            [ text "Sandbox mode - data will be reset every 5min, "
            , link []
                { url = githubOauthLink
                , label = el [ Font.underline ] (text "sign in")
                }
            , text " to keep your data!"
            ]
