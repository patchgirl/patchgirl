module ElmOption where

import           Data.Aeson

deriveElmDefOption :: Options
deriveElmDefOption =
  defaultOptions { sumEncoding = TaggedObject "tag" "contents"
                 , fieldLabelModifier = drop 1
                 }
