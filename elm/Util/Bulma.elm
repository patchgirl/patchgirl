module Util.Bulma exposing (..)

import Bulma.Columns as Bulma
import Bulma.Modifiers as Bulma

modifyColumn : Bulma.Width -> Maybe Bulma.Width -> Bulma.ColumnModifiers
modifyColumn offset width =
  let
    widths : Bulma.Devices (Maybe Bulma.Width)
    widths = Bulma.columnModifiers.widths
    modifiers = Bulma.columnModifiers
  in
    { modifiers
      | offset = offset
      , widths = { widths
                 | tablet = width
                 , desktop = width
                 , widescreen = width
                 , fullHD = width
                 }
    }
