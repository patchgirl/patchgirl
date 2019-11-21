module ElmOption where

import Data.Aeson

deriveElmDefOption :: Options
deriveElmDefOption =
  defaultOptions { sumEncoding = TaggedObject "tag" "tag"
                 , fieldLabelModifier = drop 1
                 }
