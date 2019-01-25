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

defaultModel1 =
  { name = "no name"
  , url = "{{url}}/api/people/1"
  , scheme = "HTTP"
  , method = Get
  , headers = []
  , body = ""
  , response = Nothing
  }

defaultModel2 =
  { name = "no name2"
  , url = "swapi.co/api/people/2"
  , scheme = "HTTP"
  , method = Get
  , headers = []
  , body = ""
  , response = Nothing
  }
