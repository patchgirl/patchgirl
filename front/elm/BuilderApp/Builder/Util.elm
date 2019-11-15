module BuilderApp.Builder.Util exposing (..)

import Http as Http
import BuilderApp.Model as BuilderApp

mkHeader : (String, String) -> Http.Header
mkHeader (headerKey, headerValue) = Http.header headerKey headerValue
