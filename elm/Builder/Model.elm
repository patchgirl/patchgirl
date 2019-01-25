module Builder.Model exposing (..)

import Http

type alias Model =
  { name : String
  , url : String
  , scheme : String
  , method : Method
  , headers : List(Header)
  , body : String
  , response : Maybe Response
  }

type Method = Get | Post | Put | Delete | Patch | Head | Options

type alias Header = (String, String)

type alias Response = Result Http.Error String
