{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Lime.Materials where

import Control.DeepSeq (NFData)
import Control.Lens ((^.))
import Data.Aeson
import Data.Color
import Data.Fixed (mod')
import Data.Ray (Ray (Ray, rayDirection))
import Debug.Trace
import GHC.Generics (Generic)
import Lime.Context (RenderCtx)
import Lime.Internal.Hit
import Lime.Internal.Utils (worldParse)
import Lime.Textures (TextureNode, texture)
import Linear (_x)
import Linear.Metric
import Linear.Optics (reflect, refract)
import Linear.Vector
import System.Random

data MaterialNode
  = Lambertian
      { texture :: !TextureNode
      }
  | Metal
      { fuzz :: !Float
      , texture :: !TextureNode
      }
  | Dielectric
      { ior :: !Float
      }
  | Emissive
      { strength :: !Double
      , texture :: !TextureNode
      }
  | Toon
      { texture :: !TextureNode
      }
  | Subsurface
      { texture :: !TextureNode
      , k :: !Float
      }
  deriving (Show, Generic, Eq)

instance NFData MaterialNode

instance FromJSON MaterialNode where
  parseJSON = worldParse

material :: RenderCtx -> MaterialNode -> HitData -> Ray -> Material
-------------------------------------------------------------------------------
-- Lambertian Material
-------------------------------------------------------------------------------
material ctx (Lambertian tex) hit@(HitData {normal, point}) _ rvec =
  (texture ctx tex hit, Just $ Ray point (normalize (v ^+^ normal)))
 where
  v = if rvec `dot` normal > 0 then rvec else negated rvec
-------------------------------------------------------------------------------
-- Metallic Material
-------------------------------------------------------------------------------
material ctx (Metal fuz tex) hit@(HitData {normal, point}) (Ray _ d) rvec =
  ( texture ctx tex hit
  , Just $ Ray point (reflect (normalize d) normal ^+^ (fuz *^ v))
  )
 where
  v = if sample `dot` normal > 0 then sample else negated sample
  sample = rvec
-------------------------------------------------------------------------------
-- Dielectric Material
-------------------------------------------------------------------------------
material _ (Dielectric i) (HitData {normal = n, point}) (Ray _ v) rand =
  ( pure 1
  , Just $ Ray (point ^+^ 1e-4 *^ dir) (normalize dir)
  )
 where
  v' = normalize v
  frontFace = v' `dot` n < 0
  n' = if frontFace then n else negated n
  ratio = if frontFace then 1 / i else i

  cosi = min 1 (-(v' `dot` n'))
  k = 1 - ratio * ratio * (1 - cosi * cosi)

  schlick :: Float -> Float -> Float
  schlick cosi ior =
    r0 + (1 - r0) * (1 - cosi) ^ 5
   where
    r0 = ((1 - ior) / (1 + ior)) ^ 2

  reflectProb =
    if k < 0 then 1 else schlick cosi i

  dir
    | (rand ^. _x) < reflectProb = reflect v' n'
    | otherwise = refract v' n' ratio
-- material _ (Dielectric i) (HitData {normal = n, point}) (Ray _ (v)) _ =
--   ( pure 1
--   , Just $
--       Ray point $
--         normalize $
--           if cannotRefract then reflect v n else refract v n ratio
--   )
--  where
--   ratio = if v `dot` n < 0 then 1 / i else i
--   cannotRefract = (ratio * sqrt (1 - min (negated v `dot` n) 1 ^ 2)) < 1
--------------------------------------------------------------------------------
-- Emissive Material
--------------------------------------------------------------------------------
material ctx (Emissive strength tex) hit _ _ = ((*) strength <$> texture ctx tex hit, Nothing)
--------------------------------------------------------------------------------
-- Toon Material
--------------------------------------------------------------------------------
material ctx (Toon tex) hit@(HitData n p _ _) r@(Ray o d) rvec = (c', Just $ Ray p (normalize (toonNormal n v)))
 where
  diffuse = realToFrac $ quantize $ max 0 $ abs (n `dot` d)
  c' = scale diffuse (texture ctx tex hit)
  quantize x = let k = 4 in fromIntegral (floor (x * k)) / k
  v = if rvec `dot` n > 0 then rvec else negated rvec
  toonNormal n v
    | nd > 0.66 = n
    | nd > 0.33 = normalize (n ^+^ v)
    | otherwise = v
   where
    nd = abs (n `dot` v)

--------------------------------------------------------------------------------
-- Subsurface Scattered Material
--------------------------------------------------------------------------------
material ctx (Subsurface tex k) hit@(HitData n p _ _) _ rvec =
  ( c'
  , Just (Ray p l)
  )
 where
  -- outgoing diffuse direction (unchanged)
  v = if rvec `dot` n > 0 then rvec else negated rvec
  l = normalize (n ^+^ v)

  -- wrapped diffuse
  ndotl = n `dot` l
  wrap = realToFrac $ (ndotl + k) / (1 + k)
  diffuse = realToFrac $ max 0 wrap

  -- subsurface band (terminator glow)
  scatterWidth = 0.3
  scatter =
    smoothstep 0 scatterWidth wrap
      * smoothstep (2 * scatterWidth) scatterWidth wrap

  -- color
  baseColor = texture ctx tex hit
  scatterColor = Color 1.0 0.2 0.15

  c' = (+) <$> scale diffuse baseColor <*> scale scatter scatterColor

smoothstep :: Double -> Double -> Double -> Double
smoothstep e0 e1 x =
  let t = clamp 0 1 ((x - e0) / (e1 - e0))
   in t * t * (3 - 2 * t)

clamp :: Ord a => a -> a -> a -> a
clamp lo hi = max lo . min hi

-- toon :: Color Double -> Color Double
-- toon color = let (h, s, l) = rgb2hsl color in hsl2rgb (h, s, quantize l)
--  where
--   k = 2
--   quantize x = fromIntegral (floor (x * k)) / k

-- rgb2hsl :: Color Double -> (Double, Double, Double)
-- rgb2hsl (Color r g b) =
--   let maxC = max r (max g b)
--       minC = min r (min g b)
--       l = (maxC + minC) / 2
--       d = maxC - minC

--       s
--         | d == 0 = 0
--         | otherwise = d / (1 - abs (2 * l - 1))

--       h'
--         | d == 0 = 0
--         | maxC == r = ((g - b) / d) `mod'` 6
--         | maxC == g = ((b - r) / d) + 2
--         | otherwise = ((r - g) / d) + 4

--       h = 60 * h'
--    in (h, s, l)

-- hsl2rgb :: (Double, Double, Double) -> Color Double
-- hsl2rgb (h, s, l) =
--   let c = (1 - abs (2 * l - 1)) * s
--       h' = h / 60
--       x = c * (1 - abs ((h' `mod'` 2) - 1))

--       (r1, g1, b1)
--         | h' < 1 = (c, x, 0)
--         | h' < 2 = (x, c, 0)
--         | h' < 3 = (0, c, x)
--         | h' < 4 = (0, x, c)
--         | h' < 5 = (x, 0, c)
--         | otherwise = (c, 0, x)

--       m = l - c / 2
--    in Color (r1 + m) (g1 + m) (b1 + m)
