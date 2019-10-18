module ElmOption where

import Data.Aeson

deriveElmDefOption = defaultOptions { sumEncoding = TaggedObject "tag" "tag" }
