module Textures where

import           Codec.Picture
import           Color
import           Vector

type TextureCoords = (Double, Double)

type Texture = TextureCoords -> Vector -> Color

solidColor :: Color -> Texture
solidColor color (u, v) p = color

checkerTexture :: Double -> Color -> Color -> Texture
checkerTexture scale color1 color2 (u, v) p@(Vector px py pz) = if isEven
  then solidColor color1 (u, v) p
  else solidColor color2 (u, v) p
 where
  xInt   = floor (1 / scale * px)
  yInt   = floor (1 / scale * py)
  zInt   = floor (1 / scale * pz)
  isEven = even (xInt + yInt + zInt)

imageTexture :: DynamicImage -> Texture
imageTexture img (u, v) p = if imageHeight img8 <= 0 then Color 255 0 255 else pixel
 where
  img8 = convertRGB8 img
  i = u * fromIntegral (imageWidth img8)
  j = v * fromIntegral (imageHeight img8)
  pixel =
    (\(PixelRGB8 r g b) -> Color
        ((fromIntegral (toInteger r) :: Double) / 255)
        ((fromIntegral (toInteger g) :: Double) / 255)
        ((fromIntegral (toInteger b) :: Double) / 255)
      )
      (pixelAt img8 (round i) (round j))
