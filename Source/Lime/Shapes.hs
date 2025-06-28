-- {-# LANGUAGE TemplateHaskell #-}

module Lime.Shapes where

import Language.Haskell.TH

data Foo = Foo
  { foo1 :: Double
  , foo3 :: String
  }
