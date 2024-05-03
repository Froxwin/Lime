{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Materials where

import Codec.Picture    (Image, PixelRGBF)
import Data.Aeson.Types (FromJSON (parseJSON), Parser, Value)
import Data.Map         (Map)
import GHC.Generics     (Generic)

import Color            (Color (..))
import Ray              (Ray (Ray, rayDirection))
import Textures         (TextureCoords, WorldTexture, texture)
import Utils            (worldParse)
import Vector           (Vec3, dot, normalize, reflect, refract, vadd, vmul)

-- | @Material :: Texture Coordinates -> Incident Ray -> Surface Normal -> Intersection Point -> Sample Vector -> (Pixel Color, Maybe Reflected Ray)@
type Material =
  Map String (Image PixelRGBF)
  -> TextureCoords
  -> Ray
  -> Vec3
  -> Vec3
  -> Vec3
  -> (Color Double, Maybe Ray)

data WorldMaterial
  = Lambertian
      { texture :: WorldTexture
      }
  | Metal
      { fuzz :: Double
      , texture :: WorldTexture
      }
  | Dielectric
      { ior :: Double
      }
  | Emissive
      { texture :: WorldTexture
      }
  deriving (Show, Generic, Eq)

instance FromJSON WorldMaterial where
  parseJSON :: Value -> Parser WorldMaterial
  parseJSON = worldParse

material :: WorldMaterial -> Material
-------------------------------------------------------------------------------
-- Lambertian Material
-------------------------------------------------------------------------------
material (Lambertian tex) ts coords _ n p rvec =
  (texture ts tex coords p, Just $ Ray p (rvec `vadd` n))
-------------------------------------------------------------------------------
-- Metallic Material
-------------------------------------------------------------------------------
material (Metal fuz tex) ts coords r n p rvec =
  ( texture ts tex coords p
  , Just $
      Ray p (reflect (normalize (rayDirection r)) n `vadd` (fuz `vmul` rvec))
  )
-------------------------------------------------------------------------------
-- Dielectric Material
-------------------------------------------------------------------------------
material (Dielectric i) _ _ r n p _ = (Color 1 1 1, Just $ Ray p direction)
 where
  ratio = if rayDirection r `dot` n < 0 then i else 1 / i
  direction =
    if rayDirection r `dot` n > 0
      then refract (normalize (rayDirection r)) n ratio
      else refract (normalize (rayDirection r)) n (1 / ratio)
--------------------------------------------------------------------------------
-- Emissive Material
-------------------------------------------------------------------------------
material (Emissive tex) ts coords _ _ p _ = (texture ts tex coords p, Nothing)
