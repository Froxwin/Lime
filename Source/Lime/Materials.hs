{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Lime.Materials where

import Data.Aeson.Types (FromJSON (parseJSON), Parser, Value)
import GHC.Generics (Generic)
import Linear.Metric
  ( Metric (dot, norm)
  , UnitVec (UnitVec)
  , mkUnitVec
  , normalize
  )
import Linear.Optics (reflect, refract)
import Linear.V3
import Linear.Vector (Additive ((^+^)), negated, (*^))
import System.Random

import Data.Ray (Ray (Ray, rayDirection))
import Lime.Internal.Hit
import Lime.Internal.Utils (worldParse)
import Lime.Textures (WorldTexture, texture)

import Debug.Trace

data WorldMaterial
  = Lambertian
      { texture :: !WorldTexture
      }
  | Metal
      { fuzz :: !Double
      , texture :: !WorldTexture
      }
  | Dielectric
      { ior :: !Double
      }
  | Emissive
      { texture :: !WorldTexture
      }
  deriving (Show, Generic, Eq)

instance FromJSON WorldMaterial where
  parseJSON :: Value -> Parser WorldMaterial
  parseJSON = worldParse

material :: WorldMaterial -> HitData -> Ray -> Material
-------------------------------------------------------------------------------
-- Lambertian Material
-------------------------------------------------------------------------------
material (Lambertian tex) hit@(HitData {normal, point}) _ ts rvec =
  (texture ts tex hit, Just $ Ray point (normalize (rvec ^+^ normal)))
 where

-- v =  if sample `dot` normal > 0 then sample else negated sample
-- sample =
--   normalize $ head $ filter (\t -> norm t <= 1) $ randomRs (V3 (-1) (-1) (-1), V3 1 1 1) rvec
-------------------------------------------------------------------------------
-- Metallic Material
-------------------------------------------------------------------------------
-- material (Metal fuz tex) hit@(HitData {normal, point}) (Ray _ d) ts rvec =
--   ( texture ts tex hit
--   , Just $ Ray point (reflect (normalize d) normal ^+^ (fuz *^ v))
--   )
--  where
--   v = if sample `dot` normal > 0 then sample else negated sample
--   sample = fst $ randomR (V3 (-1) (-1) (-1), V3 1 1 1) rvec
-------------------------------------------------------------------------------
-- Dielectric Material
-------------------------------------------------------------------------------
material (Dielectric i) (HitData {normal = n, point}) (Ray _ ( v)) _ _ =
  ( pure 1
  , Just $
      Ray point $
        normalize $
          if cannotRefract then reflect v n else refract v n ratio
  )
 where
  ratio = if v `dot` n < 0 then 1 / i else i
  cannotRefract = (ratio * sqrt (1 - min (negated v `dot` n) 1 ^ 2)) < 1
--------------------------------------------------------------------------------
-- Emissive Material
-------------------------------------------------------------------------------
material (Emissive tex) hit _ ts _ = (texture ts tex hit, Nothing)
