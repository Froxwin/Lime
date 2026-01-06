{-# LANGUAGE NoFieldSelectors #-}

module Lime.Internal.Hit where

import Codec.Picture
import Data.Color
import Data.Map
import Data.Ray
import Linear
import System.Random (StdGen)

data HitData = HitData
  { normal :: V3 Double
  , point :: V3 Double
  , param :: Double
  , -- , material :: Material
    coords :: TextureCoords
  }

type Material = V3 Double -> (Color Double, Maybe Ray)

-- type Material =
--   Map String (Image PixelRGBF)
--   -> HitData
--   -> Ray
--   -> StdGen -- V3 Double
--   -> (Color Double, Maybe Ray)

type TextureCoords = (Double, Double)
