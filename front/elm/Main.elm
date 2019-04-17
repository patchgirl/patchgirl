import Browser

import Window.App as Window
import Window.View as Window
import Window.Model as Window

main =
  Browser.element
    { init = Window.init
    , update = Window.update
    , subscriptions = Window.subscriptions
    , view = Window.view
    }
