{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# LANGUAGE OverloadedStrings #-}

module Textures where

import           Color                          ( Color )
import           Data.Yaml
import           GHC.Generics                   ( Generic )
import           Vector                         ( Vector(Vector) )

type TextureCoords = (Double, Double)

type Texture = TextureCoords -> Vector -> Color

class TTexture a where
  ttexture :: a -> Texture

data WorldTexture
    = SolidColor { color :: Color }
    | Checkered { scale :: Double, color1 :: Color, color2 :: Color }
    deriving ( Show, Generic, Eq )

instance FromJSON WorldTexture where

instance TTexture WorldTexture where
  ttexture :: WorldTexture -> Texture
  ttexture (SolidColor c     ) _      _                   = c
  ttexture (Checkered k c1 c2) (u, v) p@(Vector px py pz) = if isEven
    then ttexture (SolidColor c1) (u, v) p
    else ttexture (SolidColor c2) (u, v) p
   where
    xInt   = floor (1 / k * px)
    yInt   = floor (1 / k * py)
    zInt   = floor (1 / k * pz)
    isEven = even (xInt + yInt + zInt)

    -- FIXME image textures are too slow to be usable
    -- imageTexture :: DynamicImage -> Texture
    -- imageTexture img (u, v) p = pixel
    --  where
    --   img8 = convertRGB8 img
    --   i    = u * fromIntegral (imageWidth img8)
    --   j    = v * fromIntegral (imageHeight img8)
    --   pixel =
    --     (\(PixelRGB8 r g b) -> Color
    --         ((fromIntegral (toInteger r) :: Double) / 255)
    --         ((fromIntegral (toInteger g) :: Double) / 255)
    --         ((fromIntegral (toInteger b) :: Double) / 255)
    --       )
    --       (pixelAt img8 (round i) (round j))
