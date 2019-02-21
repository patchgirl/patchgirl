module Builders.Message exposing (..)

import Builder.Message as Builder

type Msg
  = SelectTab Int
  | BuilderMsg Builder.Msg
