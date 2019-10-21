import Browser

import Application.App as Application
import Application.View as Application
import Application.Model as Application

main =
  Browser.element
    { init = Application.init
    , update = Application.update
    , subscriptions = Application.subscriptions
    , view = Application.view
    }
