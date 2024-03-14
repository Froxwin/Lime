{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Materials where

import           Color                          ( Color(Color) )
import           Ray                            ( Ray(Ray, rayDirection) )
import           Textures                       ( TTexture(ttexture)
                                                , TextureCoords
                                                , WorldTexture(SolidColor)
                                                )
import           Vector                         ( Vector
                                                , dot
                                                , normalize
                                                , reflect
                                                , refract
                                                , vadd
                                                , vmul
                                                , vneg
                                                )

import           Data.Yaml                      ( FromJSON )
import           GHC.Generics                   ( Generic )

type Material
  = TextureCoords -> Ray -> Vector -> Vector -> Vector -> (Color, Maybe Ray)

class TMaterial a where
  tmaterial :: a -> Material

data WorldMaterial =
    Lambertian { texture :: WorldTexture }
  | Metal      { texture :: WorldTexture , fuzz  :: Double }
  | Dielectric { ior :: Double }
  | Emissive   { lightColor :: Color }
  deriving (Show, Generic, Eq)

instance FromJSON WorldMaterial

instance TMaterial WorldMaterial where
  tmaterial :: WorldMaterial -> Material
  tmaterial (Lambertian t) (ut, vt) _ n p v =
    (ttexture t (ut, vt) p, Just $ Ray p (v `vadd` n))

  tmaterial (Metal t f) (ut, vt) r n p v =
    ( ttexture t (ut, vt) p
    , Just $ Ray p (reflect (normalize (rayDirection r)) n `vadd` (f `vmul` v))
    )

  tmaterial (Dielectric i) _ r n p _ = (Color 1 1 1, Just $ Ray p direction)
   where
    ratio     = if rayDirection r `dot` n < 0 then 1 / i else i
    direction = if rayDirection r `dot` n > 0
      then refract (normalize (rayDirection r)) (vneg n) ratio
      else refract (normalize (rayDirection r)) n ratio

  tmaterial (Emissive lc) (u, v) _ _ p _ =
    (ttexture (SolidColor lc) (u, v) p, Nothing)
