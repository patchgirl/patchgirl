module ElmOption( deriveWithTaggedObject
                , deriveWithSingleFieldObject
                ) where

import           Data.Aeson

{-
   decoder that works for all nullary constructor or none nullary constructors
   eg:
     data Foo
       = Bar
       | Baz

   or:
     data Foo
       = Foo String
       | Bar Int

   This will not work:
     data Foo
       = Foo
       | Bar Int

-}
deriveWithTaggedObject :: Options
deriveWithTaggedObject =
  defaultOptions { sumEncoding = TaggedObject "tag" "contents"
                 , fieldLabelModifier = drop 1
                 }


{-
   decoder that works for nullary and not nullary constructor
   eg:
     data Foo
       = Bar Int
       | Baz
-}
deriveWithSingleFieldObject :: Options
deriveWithSingleFieldObject =
  defaultOptions { sumEncoding = ObjectWithSingleField
                 , fieldLabelModifier = drop 1
                 }
