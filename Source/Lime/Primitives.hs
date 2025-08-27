{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Lime.Primitives where

-- import           Codec.Wavefront     (Element (elValue), Face (Face),
--                                       FaceIndex (faceLocIndex),
--                                       Location (Location),
--                                       WavefrontOBJ (objFaces, objLocations))
import           Control.Monad       (guard)
import           Data.Aeson.Types    (FromJSON (parseJSON), Parser, Value)
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe, isNothing)
import           Data.Vector         (Vector)
import qualified Data.Vector         as V
import           GHC.Generics        (Generic)
import           Linear.Matrix
import           Linear.Metric
import           Linear.V1
import           Linear.V3
import           Linear.V4
import           Linear.Vector

import           Data.Ray
import           Lime.Internal.Hit
import           Lime.Internal.Utils (prettyError, worldParse)
import           Lime.Materials      (WorldMaterial, material)
import           Linear.Transform

import qualified Debug.Trace         as Debug
import Data.Fixed (Uni)

type Primitive = Ray -> Double -> Double -> Maybe (HitData, Material)

class IsHittable a where
  primitive :: a -> [Primitive]

data Shape
  = Sphere
      { material :: !WorldMaterial
      , transform :: ![Transform]
      }
  | Compound
      { prim1 :: Shape
      , prim2 :: Shape
      -- , 
      }
  -- | Quad
  --     { corner :: !(V3 Double)
  --     , quadU :: !(V3 Double)
  --     , quadV :: !(V3 Double)
  --     , material :: !WorldMaterial
  --     }
  | Plane
      { transform :: ![Transform]
      , -- , point :: !(V3 Double)
        -- , normal :: !(V3 Double)
        material :: !WorldMaterial
      }
  -- | Circle
  --     { center :: V3 Double
  --     , radius :: !Double
  --     , normal :: V3 Double
  --     , material :: !WorldMaterial
  --     }
  -- | Ring
  --     { center :: V3 Double
  --     , innerRadius :: !Double
  --     , outerRadius :: !Double
  --     , normal :: V3 Double
  --     , material :: !WorldMaterial
  --     }
  -- | Triangle
  --     { vertex1 :: V3 Double
  --     , vertex2 :: V3 Double
  --     , vertex3 :: V3 Double
  --     , material :: !WorldMaterial
  --     }
  -- | Mesh
  --     { objFile :: !String
  --     , material :: !WorldMaterial
  --     }
  deriving (Show, Generic, Eq)

instance FromJSON Shape where
  parseJSON :: Value -> Parser Shape
  parseJSON = worldParse

instance IsHittable Shape where
  primitive :: Shape -> [Primitive]
  -----------------------------------------------------------------------------
  -- Compound Primitive Intersection
  -----------------------------------------------------------------------------

  -----------------------------------------------------------------------------
  -- Sphere Primitive Intersection
  -----------------------------------------------------------------------------
  primitive (Sphere (material -> m) ts) = [prim']
   where
    prim' (rayTransform ts -> ray'@(Ray a ( d))) {-ray'-} tMin tMax
      | delta < 0 = Nothing
      | isWithin t = Just (dat t, m (dat t) ray')
      | isWithin t' = Just (dat t', m (dat t') ray')
      | otherwise = Nothing
     where
      ray = rayTransform ts ray'
      dat q =
          ( HitData
              (getN q)
              ((rayAt ray q))
              q
              (getUV $ getN q)
          )
      delta =
        ((d `dot` negated a) ^ 2)
          - (quadrance d * (quadrance (negated a) - 1))
      root pm = ((d `dot` negated a) `pm` sqrt delta) / quadrance d
      t = root (-)
      t' = root (+)
      tf = foldr ((!*!) . mkTransform) identity ts
      getN x =
        normalize $ transform vector (transpose $ inv44 tf) (rayAt ray x)
      isWithin x = tMin <= x && x <= tMax
      getUV (V3 x y z) = ((atan2 (-z) x + pi) / (2 * pi), acos (-y) / pi)
  -----------------------------------------------------------------------------
  -- Quadrilateral Primitive Intersection
  -----------------------------------------------------------------------------
  -- primitive _ (Quad q u v m) = [prim']
  --  where
  --   prim' ray@(Ray a dir) tMin tMax
  --     | not $ t >= tMin && t <= tMax = Nothing
  --     | otherwise = HitData normal (rayAt ray t) t (material m) <$> someThingUhh
  --    where
  --     n = u `cross` v
  --     normal = normalize n
  --     d = normal `dot` q
  --     denom = normal `dot` dir
  --     t = (d - (normal `dot` a)) / denom
  --     w = (1 / (n `dot` n)) *^ n
  --     intersection = rayAt ray t
  --     planerHitpVec = intersection ^-^ q
  --     alpha = w `dot` (planerHitpVec `cross` v)
  --     beta = w `dot` (u `cross` planerHitpVec)
  --     someThingUhh = isIn alpha beta
  --     isIn g1 g2 =
  --       guard (not (g1 < 0 || 1 < g1 || g2 < 0 || 1 < g2)) >> Just (g1, g2)
  -----------------------------------------------------------------------------
  -- Plane Primitive Intersection
  -----------------------------------------------------------------------------
  primitive (Plane ts (material -> m)) =
    pure $ \(rayTransform ts -> ray@(Ray o ( d))) tMin tMax ->
      let denom = (V3 0 1 0) `dot` d
          t = ((V3 0 1 0) `dot` ((V3 0 0 0) ^-^ o)) / denom
          tf = foldr ((!*!) . mkTransform) identity ts
          getN = normalize $ transform point (transpose $ inv44 tf) (V3 0 1 0)
       in if
            | denom == 0 -> Nothing
            | not (t >= tMin && t <= tMax) -> Nothing
            | otherwise ->
                Just $
                  (HitData getN (transform point tf (rayAt ray t)) t $ prettyError "Tried to evaluate uv for infinite surface", m (HitData getN (transform point tf (rayAt ray t)) t $ prettyError "Tried to evaluate uv for infinite surface") ray)
  -----------------------------------------------------------------------------
  -- Circle Primitive Intersection
  -----------------------------------------------------------------------------
  -- primitive o (Circle c r n m) = undefined
  -- primitive o (Circle c r n m) = pure $ \ray tMin tMax ->
  --   head (primitive o (Plane c n m)) ray tMin tMax
  --     >>= (\u -> guard (norm (u.point ^-^ c) <= r) >> Just u)
  -----------------------------------------------------------------------------
  -- Ring Primitive Intersection
  -----------------------------------------------------------------------------
  -- primitive o (Ring c r1 r2 n m) = pure $ \ray tMin tMax ->
  --   let innerCircle = head (primitive o (Circle c r1 n m)) ray tMin tMax
  --       outerCircle = head (primitive o (Circle c r2 n m)) ray tMin tMax
  --    in outerCircle >> guard (isNothing innerCircle) >> outerCircle
  -----------------------------------------------------------------------------
  -- Triangle Primitive Intersection
  -----------------------------------------------------------------------------
  -- primitive _ (Triangle v1@(V3 ax ay az) v2@(V3 bx by bz) v3@(V3 cx cy cz) m) =
  --   pure $ \ray@(Ray (V3 ox oy oz) (V3 dx dy dz)) tMin tMax ->
  --     let a =
  --           V3
  --             (V3 (ax - bx) (ax - cx) dx)
  --             (V3 (ay - by) (ay - cy) dy)
  --             (V3 (az - bz) (az - cz) dz)
  --         b = V3 (V1 (ax - ox)) (V1 (ay - oy)) (V1 (az - oz))
  --         (V3 (V1 β) (V1 γ) (V1 t)) = inv33 a !*! b
  --      in if
  --           | det33 a == 0 -> Nothing
  --           | not $ t >= tMin && t <= tMax -> Nothing
  --           | β > 0 && γ > 0 && β + γ <= 1 ->
  --               Just $
  --                 HitData ((v2 ^-^ v1) `cross` (v3 - v1)) (rayAt ray t) t (material m) (β, γ)
  --           | otherwise -> Nothing
  -----------------------------------------------------------------------------
  -- Triangle Mesh Intersection
  -----------------------------------------------------------------------------
  -- primitive objs (Mesh k m) =
  --   concat $
  --     V.toList $
  --       V.map (primitive objs) $
  --         makeTriangles m $
  --           fromMaybe
  --             (prettyError $ "Object `" ++ k ++ "` not defined")
  --             (objs M.!? k)

-- makeTriangles :: WorldMaterial -> WavefrontOBJ -> Vector Shape
-- makeTriangles m obj =
--   V.map
--     ( ( \case
--           [v1, v2, v3] -> Triangle v1 v2 v3 m
--           _ -> error "what"
--       )
--         . map
--           ( (\(Location x y z _) -> realToFrac <$> V3 x y z)
--               . ((objLocations obj V.!) . (1 `subtract`))
--           )
--         . ( \(Face i1 i2 i3 xis) ->
--               if not $ null xis
--                 then prettyError "Invalid object (possibly not triangulated)"
--                 else map faceLocIndex [i1, i2, i3]
--           )
--         . elValue
--     )
--     $ objFaces obj
