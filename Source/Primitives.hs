{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE TupleSections    #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Primitives where

import           Codec.Wavefront  (Element (elValue), Face (Face),
                                   FaceIndex (faceLocIndex),
                                   Location (Location),
                                   WavefrontOBJ (objFaces, objLocations))
import           Control.Monad    (guard)
import           Data.Aeson.Types (FromJSON (parseJSON), Parser, Value)
import           Data.Map         (Map)
import qualified Data.Map         as M
import           Data.Maybe       (fromMaybe, isNothing)
import           Data.Vector      (Vector)
import qualified Data.Vector      as V
import           GHC.Generics     (Generic)

import           Materials        (Material, WorldMaterial, material)
import           Ray              (Ray (Ray), rayAt)
import           Textures         (TextureCoords)
import           Utils            (prettyError, worldParse)
import           Vector           (Vec3 (Vec3), cross, dot, magnitude,
                                   magnitudeSquare, normalize, vmul, vneg, vsub)

type Primitive =
  Ray
  -> Double
  -> Double
  -> Maybe (Vec3, Vec3, Double, Material, TextureCoords)

class TWorldObject a where
  primitive :: Map String WavefrontOBJ -> a -> [Primitive]

data WorldObject
  = Sphere
      { center :: Vec3
      , radius :: Double
      , material :: WorldMaterial
      }
  | Quad
      { corner :: Vec3
      , quadU :: Vec3
      , quadV :: Vec3
      , material :: WorldMaterial
      }
  | Plane
      { point :: Vec3
      , normal :: Vec3
      , material :: WorldMaterial
      }
  | Circle
      { center :: Vec3
      , radius :: Double
      , normal :: Vec3
      , material :: WorldMaterial
      }
  | Ring
      { center :: Vec3
      , innerRadius :: Double
      , outerRadius :: Double
      , normal :: Vec3
      , material :: WorldMaterial
      }
  | Triangle
      { vertex1 :: Vec3
      , vertex2 :: Vec3
      , vertex3 :: Vec3
      , material :: WorldMaterial
      }
  | Mesh
      { objFile :: String
      , material :: WorldMaterial
      }
  deriving (Show, Generic, Eq)

instance FromJSON WorldObject where
  parseJSON :: Value -> Parser WorldObject
  parseJSON = worldParse

instance TWorldObject WorldObject where
  primitive :: Map String WavefrontOBJ -> WorldObject -> [Primitive]
  -----------------------------------------------------------------------------
  -- Sphere Primitive Intersection
  -----------------------------------------------------------------------------
  primitive _ (Sphere c r m) = [prim']
   where
    prim' ray@(Ray a d) tMin tMax
      | delta < 0 = Nothing
      | t' < t =
          guard (isWithin t')
            >> Just (normal t', point t', t', material m, getUV $ nrm t')
      | otherwise =
          guard (isWithin t)
            >> Just (normal t, point t, t, material m, getUV $ nrm t)
     where
      nrm q = if d `dot` normal q < 0 then normal q else vneg $ normal q
      delta =
        ((d `dot` (a `vsub` c)) ^ 2)
          - (magnitudeSquare d * (magnitudeSquare (a `vsub` c) - r ^ 2))
      root pm = ((-(d `dot` (a `vsub` c))) `pm` sqrt delta) / magnitude d ^ 2
      t = root (+)
      t' = root (-)
      point = rayAt ray
      normal x = normalize (point x `vsub` c)
      isWithin x = x >= tMin && x <= tMax
      getUV (Vec3 px py pz) = (phi / (2 * pi), theta / pi)
       where
        theta = acos (-py)
        phi = (atan2 (-pz) px) + pi
  -----------------------------------------------------------------------------
  -- Quadrilateral Primitive Intersection
  -----------------------------------------------------------------------------
  primitive _ (Quad q u v m) = [prim']
   where
    prim' ray@(Ray a dir) tMin tMax
      | not $ t >= tMin && t <= tMax = Nothing
      | otherwise = (normal,rayAt ray t,t,material m,) <$> someThingUhh
     where
      n = u `cross` v
      normal = normalize n
      d = normal `dot` q
      denom = normal `dot` dir
      t = (d - (normal `dot` a)) / denom
      w = (1 / (n `dot` n)) `vmul` n
      intersection = rayAt ray t
      planerHitpVec = intersection `vsub` q
      alpha = w `dot` (planerHitpVec `cross` v)
      beta = w `dot` (u `cross` planerHitpVec)
      someThingUhh = isIn alpha beta
      isIn g1 g2 =
        guard (not (g1 < 0 || 1 < g1 || g2 < 0 || 1 < g2)) >> Just (g1, g2)
  -----------------------------------------------------------------------------
  -- Plane Primitive Intersection
  -----------------------------------------------------------------------------
  primitive _ (Plane q n m) = [prim']
   where
    prim' ray@(Ray a dir) tMin tMax
      | denom == 0 = Nothing
      | not (t >= tMin && t <= tMax) = Nothing
      | otherwise = Just (normalize n, rayAt ray t, t, material m, (1, 2))
     where
      denom = n `dot` dir
      t = (n `dot` (q `vsub` a)) / denom
  -----------------------------------------------------------------------------
  -- Circle Primitive Intersection
  -----------------------------------------------------------------------------
  primitive o (Circle c r n m) = [prim']
   where
    prim' ray tMin tMax =
      head (primitive o (Plane c n m)) ray tMin tMax
        >>= ( \u@(_, q, _, _, _) -> guard (magnitude (q `vsub` c) <= r) >> Just u
            )
  -----------------------------------------------------------------------------
  -- Ring Primitive Intersection
  -----------------------------------------------------------------------------
  primitive o (Ring c r1 r2 n m) = [prim']
   where
    prim' ray tMin tMax =
      outerCircle >> guard (isNothing innerCircle) >> outerCircle
     where
      innerCircle = head (primitive o (Circle c r1 n m)) ray tMin tMax
      outerCircle = head (primitive o (Circle c r2 n m)) ray tMin tMax
  -----------------------------------------------------------------------------
  -- Triangle Primitive Intersection
  -----------------------------------------------------------------------------
  primitive _ (Triangle v1 v2 v3 m) = [prim']
   where
    prim' ray@(Ray o dir) tMin tMax
      | denom == 0 = Nothing
      | not $ isWithin t = Nothing
      | isInTriangle = Just (normal, p, t, material m, (u, v))
      | otherwise = Nothing
     where
      u = normal `dot` (normalize c1)
      v = normal `dot` (normalize c2)
      a = v2 `vsub` v1
      b = v3 `vsub` v1
      normal = normalize $ a `cross` b
      isWithin x = x >= tMin && x <= tMax
      denom = normal `dot` dir
      d = normal `dot` v1
      t = (d - (normal `dot` o)) / denom
      p = rayAt ray t
      edge0 = v2 `vsub` v1
      edge1 = v3 `vsub` v2
      edge2 = v1 `vsub` v3
      c0 = p `vsub` v1
      c1 = p `vsub` v2
      c2 = p `vsub` v3
      isInTriangle =
        (dot normal (cross edge0 c0) > 0)
          && (dot normal (cross edge1 c1) > 0)
          && (dot normal (cross edge2 c2) > 0)
  -----------------------------------------------------------------------------
  -- Triangle Mesh Intersection
  -----------------------------------------------------------------------------
  primitive objs (Mesh k m) =
    concat $ V.toList $ V.map (primitive objs) $ makeTriangles m obj
   where
    obj =
      fromMaybe (prettyError $ "Object `" ++ k ++ "` not defined") (objs M.!? k)

makeTriangles :: WorldMaterial -> WavefrontOBJ -> Vector WorldObject
makeTriangles m obj =
  V.map
    ( ( \case
          [v1, v2, v3] -> Triangle v1 v2 v3 m
          _ -> error "what"
      )
        . map
          ( ( \(Location x y z _) ->
                Vec3 (realToFrac x) (realToFrac y) (realToFrac z)
            )
              . ((objLocations obj V.!) . (1 `subtract`))
          )
        . ( \(Face i1 i2 i3 xis) ->
              if not $ null xis
                then prettyError "Invalid object (possibly not triangulated)"
                else map faceLocIndex [i1, i2, i3]
          )
        . elValue
    )
    $ objFaces obj
