module HttpRequestValidity exposing (..)

import Message exposing (Msg)

type alias Model =
  { urlValid : Bool,
    httpHeadersValid : Bool
  }
