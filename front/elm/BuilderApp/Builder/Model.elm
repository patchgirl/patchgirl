module BuilderApp.Builder.Model exposing (..)

import Http

type alias Model =
  { url : String
  , method : Method
  , headers : List(Header)
  , body : String
  , response : Maybe Response
  }

type Method = Get | Post | Put | Delete | Patch | Head | Options
type alias Header = (String, String)
type alias Response = Result Http.Error String

defaultBuilder =
  { url = ""
  , method = Get
  , headers = []
  , body = ""
  , response = Nothing
  }

defaultModel1 =
  { url = "{{url}}/api/people/1"
  , method = Get
  , headers = []
  , body = ""
  , response = Nothing
  }

defaultModel =
  { url = "swapi.co/api/people/2"
  , method = Get
  , headers = []
  , body = ""
  , response = Nothing
  }
