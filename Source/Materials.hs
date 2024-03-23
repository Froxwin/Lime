{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# LANGUAGE DataKinds #-}

module Materials where

import           Color                          ( Color(Color) )
import           Data.Aeson.Types               ( FromJSON(parseJSON)
                                                , Parser
                                                , Value
                                                )
import           GHC.Generics                   ( Generic )
import           Ray                            ( Ray(Ray, rayDirection) )
import           Textures                       ( TTexture(texture)
                                                , TextureCoords
                                                , WorldTexture(SolidColor)
                                                )
import           Utils                          ( worldParse )
import           Vector                         ( Vector
                                                , dot
                                                , normalize
                                                , reflect
                                                , refract
                                                , vadd
                                                , vmul
                                                , vneg
                                                )

type Material
  = TextureCoords -> Ray -> Vector -> Vector -> Vector -> (Color, Maybe Ray)

class TMaterial a where
    material :: a -> Material

data WorldMaterial
    = Lambertian
        { matTexture :: WorldTexture
        }
    | Metal
        { matFuzz    :: Double
        , matTexture :: WorldTexture
        }
    | Dielectric
        { matIor :: Double
        }
    | Emissive
        { matEmissionColor :: Color
        }
    deriving ( Show, Generic, Eq )

instance FromJSON WorldMaterial where
  parseJSON :: Value -> Parser WorldMaterial
  parseJSON = worldParse 3

instance TMaterial WorldMaterial where
  material :: WorldMaterial -> Material
  -----------------------------------------------------------------------------
  -- Lambertian Material
  -----------------------------------------------------------------------------
  material (Lambertian tex) coords _ n p rvec =
    (texture tex coords p, Just $ Ray p (rvec `vadd` n))
  -----------------------------------------------------------------------------
  -- Metallic Material
  -----------------------------------------------------------------------------
  material (Metal fuz tex) coords r n p rvec =
    ( texture tex coords p
    , Just $ Ray
      p
      (reflect (normalize (rayDirection r)) n `vadd` (fuz `vmul` rvec))
    )
  -----------------------------------------------------------------------------
  -- Dielectric Material
  -----------------------------------------------------------------------------
  material (Dielectric i) _ r n p _ = (Color 1 1 1, Just $ Ray p direction)
   where
    ratio     = if rayDirection r `dot` n < 0 then 1 / i else i
    direction = if rayDirection r `dot` n > 0
      then refract (normalize (rayDirection r)) (vneg n) ratio
      else refract (normalize (rayDirection r)) n ratio
  -----------------------------------------------------------------------------
  -- Emissive Material
  -----------------------------------------------------------------------------
  material (Emissive lc) coords _ _ p _ =
    (texture (SolidColor lc) coords p, Nothing)
