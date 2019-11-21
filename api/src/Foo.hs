{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}

module Foo where

import Control.Lens hiding (element)
import Control.Lens.TH

data A = A { _foo :: String } deriving (Show)
data B = B { _foo :: String } deriving (Show)

$(makeFieldsNoPrefix ''A)
$(makeFieldsNoPrefix ''B)
