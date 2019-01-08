module Builder.RequestValidity exposing (..)

import Builder.Message exposing (Msg)

type alias Model =
  { urlValid : Bool,
    httpHeadersValid : Bool
  }
