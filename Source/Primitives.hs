{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Primitives where

import           Data.Maybe                     ( fromJust
                                                , isNothing
                                                )
import           Data.Yaml                      ( FromJSON )
import           GHC.Generics                   ( Generic )
import           Materials                      ( Material
                                                , TMaterial(tmaterial)
                                                , WorldMaterial
                                                )
import           Ray                            ( Ray(Ray)
                                                , rayAt
                                                )
import           Textures                       ( TextureCoords )
import           Vector                         ( Vector(Vector)
                                                , cross
                                                , dot
                                                , magnitude
                                                , normalize
                                                , vmul
                                                , vsub
                                                )

type Primitive
  = Ray -> Double -> Double -> Maybe (Vector, Vector, Material, TextureCoords)

class TWorldObject a where
    tprimitive :: a -> Primitive

data WorldObject =
    Sphere { center :: Vector, radius :: Double, material :: WorldMaterial }
  | Quad   { corner :: Vector, quadU :: Vector, quadV :: Vector, material :: WorldMaterial }
  deriving (Show, Generic)

instance FromJSON WorldObject

instance TWorldObject WorldObject where
  tprimitive :: WorldObject -> Primitive
  tprimitive (Sphere c r m) ray@(Ray a d) tMin tMax
    | delta < 0 = Nothing
    | otherwise = if t' < t
      then
        (if isWithin t'
          then Just (normal t', point t', tmaterial m, getUV $ normal t')
          else Nothing
        )
      else
        (if isWithin t
          then Just (normal t, point t, tmaterial m, getUV $ normal t)
          else Nothing
        )
   where
    delta =
      ((d `dot` (a `vsub` c)) ^ 2)
        - (magnitude d ^ 2)
        * ((magnitude (a `vsub` c) ^ 2) - r ^ 2)
    root pm = ((-(d `dot` (a `vsub` c))) `pm` sqrt delta) / magnitude d ^ 2
    t     = root (+)
    t'    = root (-)
    point = rayAt ray
    normal x = normalize (point x `vsub` c)
    isWithin x = x >= tMin && x <= tMax
    getUV (Vector px py pz) = (phi / (2 * pi), theta / pi)
     where
      theta = acos (-py)
      phi   = atan2 (-pz) px + pi

  tprimitive (Quad q u v m) ray@(Ray a dir) tMin tMax
    | not $ isWithin t = Nothing
    | isNothing someThingUhh = Nothing
    | otherwise = Just (normal, rayAt ray t, tmaterial m, fromJust someThingUhh)
   where
    isWithin x = x >= tMin && x <= tMax
    n             = u `cross` v
    normal        = normalize n
    d             = normal `dot` q
    denom         = normal `dot` dir
    t             = (d - (normal `dot` a)) / denom
    w             = (1 / (n `dot` n)) `vmul` n
    intersection  = rayAt ray t
    planerHitpVec = intersection `vsub` q
    alpha         = w `dot` (planerHitpVec `cross` v)
    beta          = w `dot` (u `cross` planerHitpVec)
    someThingUhh  = isIn alpha beta
    isIn g1 g2 | (g1 < 0) || (1 < g1) || (g2 < 0) || (1 < g2) = Nothing
               | otherwise = Just (g1, g2)
