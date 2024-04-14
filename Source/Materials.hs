{-# OPTIONS_GHC -Wno-partial-fields #-}

module Materials where

import Data.Aeson.Types (FromJSON (parseJSON), Parser, Value)
import GHC.Generics     (Generic)

import Color            (Color (..))
import Ray              (Ray (Ray, rayDirection))
import Textures         (TextureCoords, WorldTexture, texture)
import Utils            (worldParse)
import Vector           (Vec3, dot, normalize, reflect, refract, vadd, vmul)

-- | @Material :: Texture Coordinates -> Incident Ray -> Surface Normal -> Intersection Point -> Sample Vector -> (Pixel Color, Maybe Reflected Ray)@
type Material
  = TextureCoords -> Ray -> Vec3 -> Vec3 -> Vec3 -> (Color Double, Maybe Ray)

data WorldMaterial
  = Lambertian
      { matTexture :: WorldTexture
      }
  | Metal
      { matFuzz :: Double
      , matTexture :: WorldTexture
      }
  | Dielectric
      { matIor :: Double
      }
  | Emissive
      { matTexture :: WorldTexture
      }
  deriving (Show, Generic, Eq)

instance FromJSON WorldMaterial where
  parseJSON :: Value -> Parser WorldMaterial
  parseJSON = worldParse 3

material :: WorldMaterial -> Material
-------------------------------------------------------------------------------
-- Lambertian Material
-------------------------------------------------------------------------------
material (Lambertian tex) coords _ n p rvec =
  (texture tex coords p, Just $ Ray p (rvec `vadd` n))
-------------------------------------------------------------------------------
-- Metallic Material
-------------------------------------------------------------------------------
material (Metal fuz tex) coords r n p rvec =
  ( texture tex coords p
  , Just
    $ Ray p (reflect (normalize (rayDirection r)) n `vadd` (fuz `vmul` rvec))
  )
-------------------------------------------------------------------------------
-- Dielectric Material
-------------------------------------------------------------------------------
material (Dielectric i) _ r n p _ = (Color 1 1 1, Just $ Ray p direction)
 where
  ratio     = if rayDirection r `dot` n < 0 then i else 1 / i
  direction = if rayDirection r `dot` n > 0
    then refract (normalize (rayDirection r)) n ratio
    else refract (normalize (rayDirection r)) n ratio
--------------------------------------------------------------------------------
-- Emissive Material
-------------------------------------------------------------------------------
material (Emissive tex) coords _ _ p _ = (texture tex coords p, Nothing)
