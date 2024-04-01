{-# OPTIONS_GHC -Wno-partial-fields #-}

module Textures where

import Color            (Color)
import Data.Aeson.Types (FromJSON (parseJSON), Parser, Value)
import GHC.Generics     (Generic)
import Utils            (worldParse)
import Vector           (Vector (Vector))

type Texture = TextureCoords -> Vector -> Color
type TextureCoords = (Double, Double)

class TTexture a where
  texture :: a -> Texture

data WorldTexture
  = SolidColor
      { texColor :: Color
      }
  | Checkered
      { texScale  :: Double,
        texColor1 :: Color,
        texColor2 :: Color
      }
  deriving (Show, Generic, Eq)

instance FromJSON WorldTexture where
  parseJSON :: Value -> Parser WorldTexture
  parseJSON = worldParse 3

instance TTexture WorldTexture where
  texture :: WorldTexture -> Texture
  -----------------------------------------------------------------------------
  -- Solid Texture
  -----------------------------------------------------------------------------
  texture (SolidColor color) _ _ = color
  -----------------------------------------------------------------------------
  -- Checkered Texture
  -----------------------------------------------------------------------------
  texture (Checkered k c1 c2) coords p
    | isEven p  = texture (SolidColor c1) coords p
    | otherwise = texture (SolidColor c2) coords p
   where
    qInt q = floor (1 / k * q)
    isEven (Vector px py pz) = even (qInt px + qInt py + qInt pz)
  -----------------------------------------------------------------------------
  -- Image Texture
  -----------------------------------------------------------------------------
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
