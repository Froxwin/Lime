{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lime.Internal.Utils where

import Data.Aeson.Types
  ( FromJSON
  , GFromJSON
  , Options (..)
  , Parser
  , Value
  , Zero
  , camelTo2
  , defaultOptions
  , genericParseJSON
  , parseJSON
  )
import GHC.Generics (Generic (Rep))
import Linear.V3 (V3 (..))

import Data.Color (Color)

-- getPix :: [Color Double] -> Int -> Int -> Int -> Color Double
-- getPix img w x y = img !! ((w * y) + x)

worldParse :: (Generic a, GFromJSON Zero (Rep a)) => Value -> Parser a
worldParse =
  genericParseJSON
    defaultOptions
      { fieldLabelModifier = camelTo2 '_'
      , constructorTagModifier = camelTo2 '-'
      , rejectUnknownFields = True
      }

prettyError :: [Char] -> a
prettyError err = errorWithoutStackTrace $ "\ESC[1;31m" ++ err ++ "\ESC[0m"

instance FromJSON a => FromJSON (V3 a) where
  parseJSON :: Value -> Parser (V3 a)
  parseJSON v = do
    [x, y, z] <- parseJSON v
    return $ V3 x y z
