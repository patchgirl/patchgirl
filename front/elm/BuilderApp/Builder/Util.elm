module BuilderApp.Builder.Util exposing (..)

import Http as Http
import BuilderApp.Model as BuilderApp

mkHeader : BuilderApp.Header -> Http.Header
mkHeader (headerKey, headerValue) = Http.header headerKey headerValue
