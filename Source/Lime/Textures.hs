{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Lime.Textures where

import Codec.Picture
  ( Image (imageHeight, imageWidth)
  , Pixel (pixelAt)
  , PixelRGBF (..)
  )
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON (parseJSON))
import Data.Color (Color (..))
import Data.Fixed (mod')
import Data.Map ((!?))
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Lime.Context (RenderCtx (textures))
import Lime.Internal.Hit (HitData (HitData, coords, normal, point))
import Lime.Internal.Utils (prettyError, worldParse)
import Linear (V3 (V3), (*^))

type Texture = HitData -> Color Double

data TextureNode
  = SolidColor
      { color :: !(Color Double)
      }
  | Checkered
      { scale :: !Double
      , texture1 :: !TextureNode
      , texture2 :: !TextureNode
      }
  | ImageTexture
      { image :: !String
      , style :: !FillStyle
      }
  | NormalMap
  | UVMap
  deriving (Show, Generic, Eq)

instance NFData TextureNode
instance NFData FillStyle

data FillStyle = Stretch | Repeat {scale :: !Double}
  deriving (Show, Generic, Eq)

instance FromJSON FillStyle where
  parseJSON = worldParse

instance FromJSON TextureNode where
  parseJSON = worldParse

texture :: RenderCtx -> TextureNode -> Texture
texture _ (SolidColor color) _ = color
texture ctx (Checkered k t1 t2) hit@(HitData {point}) =
  texture
    ctx
    ( if even (let (V3 qx qy qz) = ((1 / k) *^ point) in floor qx + floor qy + floor qz)
        then t1
        else t2
    )
    hit
texture ctx (ImageTexture key style) HitData {coords = (u, v)} = case style of
  Stretch -> pixel (pixelAt img (floor i) (floor j))
  Repeat s ->
    let wrap q = floor $ q `mod'` fromIntegral (imageWidth img)
     in pixel (pixelAt img (wrap (i * realToFrac s)) (wrap (j * realToFrac s)))
 where
  img =
    fromMaybe (prettyError $ "Texture `" ++ key ++ "` not defined") $
      ctx.textures !? key
  i = u * fromIntegral (imageWidth img)
  j = (1 - v) * fromIntegral (imageHeight img)
  pixel (PixelRGBF r g b) = realToFrac <$> Color r g b
texture _ NormalMap (HitData {normal}) =
  (* 0.5) . (+ 1)
    <$> let (V3 r g b) = normal in Color (realToFrac r) (realToFrac g) (realToFrac b)
texture _ UVMap (HitData {coords = (u, v)}) = Color (realToFrac u) 0 (realToFrac v)
