{-# LANGUAGE FlexibleContexts #-}

module Utils where

import Data.Aeson.Types (GFromJSON,
                         Options (constructorTagModifier, fieldLabelModifier, rejectUnknownFields),
                         Parser, Value, Zero, camelTo2, defaultOptions,
                         genericParseJSON)
import GHC.Generics     (Generic (Rep))

worldParse :: (Generic a, GFromJSON Zero (Rep a)) => Int -> Value -> Parser a
worldParse n = genericParseJSON defaultOptions
  { fieldLabelModifier     = camelTo2 '-' . drop n
  , constructorTagModifier = camelTo2 '-'
  , rejectUnknownFields    = True
  }
