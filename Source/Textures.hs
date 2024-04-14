{-# OPTIONS_GHC -Wno-partial-fields #-}

module Textures where

import Data.Aeson.Types (FromJSON (parseJSON), Parser, Value)
import GHC.Generics     (Generic)

import Color            (Color (..))
import Utils            (worldParse)
import Vector           (Vec3 (Vec3))

type Texture = TextureCoords -> Vec3 -> Color Double
type TextureCoords = (Double, Double)

data WorldTexture
  = SolidColor
      { texColor :: Color Double
      }
  | Checkered
      { texScale :: Double
      , texColor1 :: Color Double
      , texColor2 :: Color Double
      }
  deriving (Show, Generic, Eq)

instance FromJSON WorldTexture where
  parseJSON :: Value -> Parser WorldTexture
  parseJSON = worldParse 3

texture :: WorldTexture -> Texture
-------------------------------------------------------------------------------
-- Solid Texture
-------------------------------------------------------------------------------
texture (SolidColor color) _ _ = color
-------------------------------------------------------------------------------
-- Checkered Texture
-------------------------------------------------------------------------------
texture (Checkered k c1 c2) coords p
  | isEven p  = texture (SolidColor c1) coords p
  | otherwise = texture (SolidColor c2) coords p
 where
  qInt q = floor (1 / k * q)
  isEven (Vec3 px py pz) = even (qInt px + qInt py + qInt pz)

-------------------------------------------------------------------------------
-- Image Texture
-------------------------------------------------------------------------------
-- FIXME image textures are too slow to be usable
-- texture (ImageTexture key) (TextureCoords (u, v)) _ = pixel
--  where
--   img = snd $ head $ filter (\(x,_) -> x == key) texs
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
