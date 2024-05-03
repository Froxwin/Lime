{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Textures where

import Codec.Picture    (Image (imageHeight, imageWidth), Pixel (pixelAt),
                         PixelRGBF (..))
import Data.Aeson.Types (FromJSON (parseJSON), Parser, Value)
import Data.Map         (Map, (!?))
import Data.Maybe       (fromMaybe)
import GHC.Generics     (Generic)

import Color            (Color (..))
import Utils            (prettyError, worldParse)
import Vector           (Vec3 (Vec3))

type Texture = TextureCoords -> Vec3 -> Color Double

type TextureCoords = (Double, Double)

data WorldTexture
  = SolidColor
      { color :: Color Double
      }
  | Checkered
      { scale :: Double
      , color1 :: Color Double
      , color2 :: Color Double
      }
  | ImageTexture
      { image :: String
      }
  deriving (Show, Generic, Eq)

instance FromJSON WorldTexture where
  parseJSON :: Value -> Parser WorldTexture
  parseJSON = worldParse

texture :: Map String (Image PixelRGBF) -> WorldTexture -> Texture
-------------------------------------------------------------------------------
-- Solid Texture
-------------------------------------------------------------------------------
texture _ (SolidColor color) _ _ = color
-------------------------------------------------------------------------------
-- Checkered Texture
-------------------------------------------------------------------------------
texture ts (Checkered k c1 c2) coords p
  | isEven p = texture ts (SolidColor c1) coords p
  | otherwise = texture ts (SolidColor c2) coords p
 where
  qInt q = floor (1 / k * q)
  isEven (Vec3 px py pz) = even (qInt px + qInt py + qInt pz)
-------------------------------------------------------------------------------
-- Image Texture
-------------------------------------------------------------------------------
texture ts (ImageTexture key) (u, v) _ = pixel
 where
  img =
    fromMaybe (prettyError $ "Texture `" ++ key ++ "` not defined") $ ts !? key
  i = floor (u * fromIntegral (imageWidth img)) :: Int
  j = floor ((1 - v) * fromIntegral (imageHeight img)) :: Int
  pixel =
    (\(PixelRGBF r g b) -> realToFrac <$> Color r g b) (pixelAt img i j)
      :: Color Double
