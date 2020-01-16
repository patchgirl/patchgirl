module Main exposing (..)

import Browser

import Application.App as Application

main =
  Browser.application
    { init = Application.init
    , update = Application.update
    , subscriptions = Application.subscriptions
    , view = Application.view
    , onUrlRequest = Application.LinkClicked
    , onUrlChange = Application.UrlChanged
    }
