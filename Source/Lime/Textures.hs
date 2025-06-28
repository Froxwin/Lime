{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Lime.Textures where

import Codec.Picture       (Image (imageHeight, imageWidth), Pixel (pixelAt),
                            PixelRGBF (..))
import Data.Aeson.Types    (FromJSON (parseJSON), Parser, Value)
import Data.Fixed          (mod')
import Data.Map            (Map, (!?))
import Data.Maybe          (fromMaybe)
import GHC.Generics        (Generic)
import Linear.Metric       (Metric (dot))

import Data.Color          (Color (..))
import Lime.Internal.Hit   (HitData (HitData, coords, normal, point))
import Lime.Internal.Utils (prettyError, worldParse)
import Linear              (V3 (..))

-- | A texture is just represented as a function that takes information about
-- the intersection and gives the color of the point of intersection
-- (not the pixel).
type Texture = HitData -> Color Double

data WorldTexture
  = SolidColor
      { color :: !(Color Double)
      }
  | Checkered
      { scale :: !Double
      , texture1 :: !WorldTexture
      , texture2 :: !WorldTexture
      }
  | ImageTexture
      { image :: !String
      , style :: !TextureFillStyle
      }
  | NormalMap
  deriving (Show, Generic, Eq)

data TextureFillStyle = Stretch | Repeat {scale :: !Double}
  deriving (Show, Generic, Eq)

instance FromJSON TextureFillStyle where
  parseJSON :: Value -> Parser TextureFillStyle
  parseJSON = worldParse

instance FromJSON WorldTexture where
  parseJSON :: Value -> Parser WorldTexture
  parseJSON = worldParse

texture :: Map String (Image PixelRGBF) -> WorldTexture -> Texture
-------------------------------------------------------------------------------
-- Solid Texture
-------------------------------------------------------------------------------
texture _ (SolidColor color) _ = color
-------------------------------------------------------------------------------
-- Checkered Texture
-------------------------------------------------------------------------------
{-
[Alternate implementation]
texture ts (Checkered k t1 t2) hit@(HitData {point, coords}) =
  texture ts (if (sx * sy * sz) < 0 then t1 else t2) hit {coords = (abs sx, abs sy)}
  where
    (V3 sx sy sz) = sin . (k *) <$> point
-}
texture ts (Checkered k t1 t2) hit@(HitData {point}) =
  texture
    ts
    (if even ((floor . (1 / k *) <$> point) `dot` pure 1) then t1 else t2)
    hit
-------------------------------------------------------------------------------
-- Image Texture
-------------------------------------------------------------------------------
texture ts (ImageTexture key style) HitData {coords = (u, v)} = case style of
  Stretch -> pixel (pixelAt img (floor i) (floor j))
  Repeat s ->
    let wrap q = floor $ q `mod'` fromIntegral (imageWidth img)
     in pixel (pixelAt img (wrap (i * s)) (wrap (j * s)))
 where
  img =
    fromMaybe (prettyError $ "Texture `" ++ key ++ "` not defined") $ ts !? key
  i = u * fromIntegral (imageWidth img)
  j = (1 - v) * fromIntegral (imageHeight img)
  pixel (PixelRGBF r g b) = realToFrac <$> Color r g b
-------------------------------------------------------------------------------
-- Normal Mapped Texture
-------------------------------------------------------------------------------
texture _ NormalMap (HitData {normal = V3 r g b}) =
  (* 0.5) . (+ 1) <$> Color r g b
-------------------------------------------------------------------------------
