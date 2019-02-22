module Util.Maybe exposing (..)

exists : (Maybe a) -> (a -> Bool) -> Bool
exists m f =
  case m of
    Just a -> f a
    Nothing -> False
