module Main exposing (..)

import Browser

import Application.App as Application

main =
  Browser.element
    { init = Application.init
    , update = Application.update
    , subscriptions = Application.subscriptions
    , view = Application.view
    }
