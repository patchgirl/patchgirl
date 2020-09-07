module BuilderUtil exposing (..)

import Application.Type exposing (..)
import Util exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Page exposing(..)


-- * view


-- ** close builder view


closeBuilderView : Element a
closeBuilderView =
    link []
        { url = href (ReqPage (LandingView DefaultView))
        , label =
            iconWithAttr { defaultIconAttribute
                             | title = ""
                             , icon = "clear"
                         }
        }
