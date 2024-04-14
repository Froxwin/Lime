{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Primitives where

import           Codec.Wavefront  (Element (elValue), Face (Face),
                                   FaceIndex (faceLocIndex),
                                   Location (Location),
                                   WavefrontOBJ (objFaces, objLocations))
import           Data.Aeson.Types (FromJSON (parseJSON), Parser, Value)
import           Data.Map         (Map)
import qualified Data.Map         as M
import           Data.Maybe       (fromMaybe)
import           Data.Vector      (Vector)
import qualified Data.Vector      as V
import           GHC.Generics     (Generic)

import           Materials        (Material, WorldMaterial, material)
import           Ray              (Ray (Ray), rayAt)
import           Textures         (TextureCoords)
import           Utils            (worldParse)
import           Vector           (Vec3 (Vec3), cross, dot, magnitude,
                                   magnitudeSquare, normalize, vmul, vsub)

type Primitive
  =  Ray
  -> Double
  -> Double
  -> Maybe (Vec3, Vec3, Double, Material, TextureCoords)

type Hmm
  = [  Ray
    -> Double
    -> Double
    -> Maybe (Vec3, Vec3, Double, Material, TextureCoords)
    ]

class TWorldObject a where
  primitive :: Map String WavefrontOBJ -> a -> [Primitive]

-- boundingBox :: a -> AABB

data WorldObject
  = Sphere
      { primCenter :: Vec3
      , primRadius :: Double
      , primMaterial :: WorldMaterial
      }
  | Quad
      { primCorner :: Vec3
      , primQuadU :: Vec3
      , primQuadV :: Vec3
      , primMaterial :: WorldMaterial
      }
  | Circle
      { primCenter :: Vec3
      , primRadius :: Double
      , primNormal :: Vec3
      , primMaterial :: WorldMaterial
      }
  | Ring
      { primCenter :: Vec3
      , primInnerRadius :: Double
      , primOuterRadius :: Double
      , primNormal :: Vec3
      , primMaterial :: WorldMaterial
      }
  | Triangle
      { primVertex1 :: Vec3
      , primVertex2 :: Vec3
      , primVertex3 :: Vec3
      , primMaterial :: WorldMaterial
      }
  | Mesh
      { primObjFile :: String
      , primMaterial :: WorldMaterial
      }
  deriving (Show, Generic, Eq)

instance FromJSON WorldObject where
  parseJSON :: Value -> Parser WorldObject
  parseJSON = worldParse 4

instance TWorldObject WorldObject where
  primitive :: Map String WavefrontOBJ -> WorldObject -> [Primitive]
  -----------------------------------------------------------------------------
  -- Sphere Primitive Intersection
  -----------------------------------------------------------------------------
  primitive _ (Sphere c r m) = [prim']
   where
    prim' ray@(Ray a d) tMin tMax
      | delta < 0 = Nothing
      | t' < t = if isWithin t'
        then Just (normal t', point t', t', material m, getUV $ normal t')
        else Nothing
      | otherwise = if isWithin t
        then Just (normal t, point t, t, material m, getUV $ normal t)
        else Nothing
     where
      delta =
        ((d `dot` (a `vsub` c)) ^ 2)
          - (magnitudeSquare d * (magnitudeSquare (a `vsub` c) - r ^ 2))
      root pm = ((-(d `dot` (a `vsub` c))) `pm` sqrt delta) / magnitude d ^ 2
      t     = root (+)
      t'    = root (-)
      point = rayAt ray
      normal x = normalize (point x `vsub` c)
      isWithin x = x >= tMin && x <= tMax
      getUV (Vec3 px py pz) = (phi / (2 * pi), theta / pi)
       where
        theta = acos (-py)
        phi   = atan2 (-pz) px + pi
  -----------------------------------------------------------------------------
  -- Quadrilateral Primitive Intersection
  -----------------------------------------------------------------------------
  primitive _ (Quad q u v m) = [prim']
   where
    prim' ray@(Ray a dir) tMin tMax
      | not $ isWithin t = Nothing
      | otherwise = (normal, rayAt ray t, t, material m, ) <$> someThingUhh
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
      isIn g1 g2 | g1 < 0 || 1 < g1 || g2 < 0 || 1 < g2 = Nothing
                 | otherwise                            = Just (g1, g2)
  -----------------------------------------------------------------------------
  -- Circle Primitive Intersection
  -----------------------------------------------------------------------------
  primitive _ (Circle c r n m) = [prim']
   where
    prim' ray@(Ray a dir) tMin tMax
      | denom == 0       = Nothing
      | not $ isWithin t = Nothing
      | isInCircle       = Just (normal, p, t, material m, (1, 2))
      | otherwise        = Nothing
     where
      isWithin x = x >= tMin && x <= tMax
      normal     = normalize n
      d          = normal `dot` c
      denom      = normal `dot` dir
      t          = (d - (normal `dot` a)) / denom
      p          = rayAt ray t
      isInCircle = magnitude (p `vsub` c) <= r
  -----------------------------------------------------------------------------
  -- Ring Primitive Intersection
  -----------------------------------------------------------------------------
  primitive _ (Ring c r1 r2 n m) = [prim']
   where
    prim' ray@(Ray a dir) tMin tMax
      | denom == 0       = Nothing
      | not $ isWithin t = Nothing
      | isInRing         = Just (normal, p, t, material m, (1, 2))
      | otherwise        = Nothing
     where
      isWithin x = x >= tMin && x <= tMax
      normal   = normalize n
      d        = normal `dot` c
      denom    = normal `dot` dir
      t        = (d - (normal `dot` a)) / denom
      p        = rayAt ray t
      isInRing = magnitude (p `vsub` c) >= r1 && magnitude (p `vsub` c) <= r2
  -----------------------------------------------------------------------------
  -- Triangle Primitive Intersection
  -----------------------------------------------------------------------------
  primitive _ (Triangle v1 v2 v3 m) = [prim']
   where
    prim' ray@(Ray o dir) tMin tMax
      | denom == 0       = Nothing
      | not $ isWithin t = Nothing
      | isInTriangle     = Just (normal, p, t, material m, (1, 2))
      | otherwise        = Nothing
     where
      a      = v2 `vsub` v1
      b      = v3 `vsub` v1
      normal = normalize $ a `cross` b
      isWithin x = x >= tMin && x <= tMax
      denom = normal `dot` dir
      d     = normal `dot` v1
      t     = (d - (normal `dot` o)) / denom
      p     = rayAt ray t
      edge0 = v2 `vsub` v1
      edge1 = v3 `vsub` v2
      edge2 = v1 `vsub` v3
      c0    = p `vsub` v1
      c1    = p `vsub` v2
      c2    = p `vsub` v3
      isInTriangle =
        (dot normal (cross edge0 c0) > 0)
          && (dot normal (cross edge1 c1) > 0)
          && (dot normal (cross edge2 c2) > 0)
  primitive objss (Mesh k m) = ts
   where
    obj = fromMaybe
      (errorWithoutStackTrace $ concat ["Object `", k, "` not found"])
      (objss M.!? k)
    ts = concat $ V.toList $ V.map (primitive objss) $ makeTriangles m obj

makeTriangles :: WorldMaterial -> WavefrontOBJ -> Vector WorldObject
makeTriangles m obj =
  V.map
      ( (\case
          [v1, v2, v3] -> Triangle v1 v2 v3 m
          _            -> error "what"
        )
      . map
          ( (\(Location x y z _) ->
              Vec3 (realToFrac x) (realToFrac y) (realToFrac z)
            )
          . ((objLocations obj V.!) . (1 `subtract`))
          )
      . (\(Face i1 i2 i3 xis) -> if not $ null xis
          then error "Invalid object: (Possibly not triangulated)"
          else map faceLocIndex [i1, i2, i3]
        )
      . elValue
      )
    $ objFaces obj
