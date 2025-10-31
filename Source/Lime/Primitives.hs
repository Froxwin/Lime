{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

module Lime.Primitives where

import Control.Lens ((^.), _Unwrapped')
import Control.Monad (guard)
import Data.Aeson.Types
import Data.Function (on)
import Data.List (minimumBy, sortBy)
import Data.Maybe (catMaybes, fromJust, isJust)
import Data.Ord (comparing)
import GHC.Generics
import Linear.Matrix
import Linear.Metric
import Linear.V3
import Linear.V4
import Linear.Vector

import Data.Functor.Classes (eq2)
import Data.Ray
import Data.Wavefront (FaceRef (FaceRef), Object (Object))
import Debug.Trace (traceShow)
import Debug.Trace qualified
import Lime.Internal.Hit
import Lime.Internal.Utils
import Lime.Materials
import Linear (V1 (V1))
import Linear.Transform

type Primitive = Ray -> Double -> Double -> Maybe (HitData, Material)

data Geometry
  = Sphere
  | Plane
  | Circle
  | Quad
  | Triangle !(V3 (V3 Double))
  deriving (Show, Eq, Generic)

instance FromJSON Geometry where
  parseJSON :: Value -> Parser Geometry
  parseJSON = worldParse

data Shape = Shape
  { geometry :: !Geometry
  , transform :: ![Transform]
  , material :: !WorldMaterial
  }
  deriving (Show, Generic, Eq)

data Mesh = Mesh
  { path :: String
  , transform :: [Transform]
  , material :: WorldMaterial
  }
  deriving (Show, Eq, Generic)

instance FromJSON Mesh where
  parseJSON :: Value -> Parser Mesh
  parseJSON = worldParse

instance FromJSON Shape where
  parseJSON :: Value -> Parser Shape
  parseJSON = worldParse

data AABB = AABB
  { lo :: !(V3 Double)
  , hi :: !(V3 Double)
  }

data BVH
  = BVHLeaf Primitive !AABB
  | BVHNode !AABB BVH BVH

traverseBVH :: BVH -> Primitive
traverseBVH (BVHLeaf prim _) = prim
traverseBVH (BVHNode (AABB lo hi) left right) =
  \ray@(Ray o d) tMin tMax ->
    let
      t1 = (/) <$> (lo ^-^ o) <*> d
      t2 = (/) <$> (hi ^-^ o) <*> d
      tmin = ((-) 1e-6) $ maximum $ min <$> t1 <*> t2
      tmax = ((+) 1e-6) $ minimum $ max <$> t1 <*> t2
      hits =
        catMaybes
          [traverseBVH left ray tMin tMax, traverseBVH right ray tMin tMax]
     in
      guard (tmax >= tmin)
        >> case hits of
          [] -> Nothing
          [h] -> Just h
          hs -> Just $ minimumBy (compare `on` (\(q, _) -> q.param)) hs

-- traverseBVH :: BVH -> Primitive
-- traverseBVH (BVHLeaf prim _) ray tMin tMax = prim ray tMin tMax
-- traverseBVH (BVHNode (AABB lo hi) left right) ray@(Ray o d) tMin tMax =
--   let
--     t1 = (lo - o) / d
--     t2 = (hi - o) / d

--     tmin' = max tMin (maximum (min <$> t1 <*> t2))
--     tmax' = min tMax (minimum (max <$> t1 <*> t2))
--    in
--     if tmax' < tmin'
--       then Nothing
--       else case (traverseBVH left ray tmin' tmax', traverseBVH right ray tmin' tmax') of
--         (Nothing, r) -> r
--         (l, Nothing) -> l
--         (Just (dat1, p1), Just (dat2, p2)) -> if dat1.param < dat2.param then Just (dat1, p1) else Just (dat2, p2)

constructBVH :: [Shape] -> BVH
constructBVH [] = error "constructBVH: empty scene"
constructBVH shapes = buildBVH $ map (\s -> (shapeBounds s, s)) shapes

buildBVH :: [(AABB, Shape)] -> BVH
buildBVH items =
  case items of
    [] -> error "buildBVH: empty"
    [(bbox, s)] -> BVHLeaf (primitive s) bbox
    [(b1, s1), (b2, s2)] ->
      let nodeB = unionAABB b1 b2
       in BVHNode
            nodeB
            (BVHLeaf (primitive s1) b1)
            (BVHLeaf (primitive s2) b2)
    _ ->
      let
        (AABB bmin bmax) = foldl1 unionAABB (map fst items)
        -- choose longest axis
        ext = bmax - bmin
        axis = longAxis ext
        -- sort by centroid along axis and split at median
        withCentroids = [(centroid bbox, (bbox, s)) | (bbox, s) <- items]

        key (V3 x y z) = case axis of
          0 -> x
          1 -> y
          2 -> z
          _ -> error ":)"

        sorted = map snd $ sortBy (comparing (key . fst)) withCentroids
        (leftItems, rightItems) = splitAt (length sorted `div` 2) sorted
        leftBVH = buildBVH leftItems
        rightBVH = buildBVH rightItems
        leftB = bboxOfBVH leftBVH
        rightB = bboxOfBVH rightBVH
        (AABB nodeMin nodeMax) = unionAABB leftB rightB
       in
        BVHNode (AABB nodeMin nodeMax) leftBVH rightBVH

centroid :: AABB -> V3 Double
centroid (AABB lo hi) = (lo + hi) ^/ 2

-- return index of longest axis: 0 -> x, 1 -> y, 2 -> z
longAxis :: V3 Double -> Int
longAxis (V3 x y z)
  | x >= y && x >= z = 0
  | y >= x && y >= z = 1
  | otherwise = 2

unionAABB :: AABB -> AABB -> AABB
unionAABB (AABB min1 max1) (AABB min2 max2) =
  AABB (min <$> min1 <*> min2) (max <$> max1 <*> max2)

bboxOfBVH :: BVH -> AABB
bboxOfBVH = \case
  BVHLeaf _ box -> box
  BVHNode box _ _ -> box

shapeBounds :: Shape -> AABB
shapeBounds Shape {..} =
  let tf = foldr ((!*!) . mkTransform) identity transform
      localBBox = objectBBox geometry
   in transformAABB tf localBBox

objectBBox :: Geometry -> AABB
objectBBox = \case
  Sphere -> AABB (V3 (-1) (-1) (-1)) (V3 1 1 1)
  Quad -> AABB (V3 (-0.5) (-1e-3) (-0.5)) (V3 0.5 1e-3 0.5)
  Circle -> AABB (V3 (-1) (-1e-3) (-1)) (V3 1 1e-3 1)
  Plane -> AABB (V3 (-1e6) (-1e-3) (-1e6)) (V3 1e6 1e-3 1e6)
  Triangle vertices ->
    let lo = minimum vertices - pure 1e-6
        hi = maximum vertices + pure 1e-6
     in AABB lo hi

transformAABB :: M44 Double -> AABB -> AABB
transformAABB m (AABB (V3 minx miny minz) (V3 maxx maxy maxz)) =
  let corners =
        [ V3 x y z
        | x <- [minx, maxx]
        , y <- [miny, maxy]
        , z <- [minz, maxz]
        ]
      transformed = map (transform point m) corners
      xs = map (^. _x) transformed
      ys = map (^. _y) transformed
      zs = map (^. _z) transformed
   in AABB
        (V3 (minimum xs) (minimum ys) (minimum zs))
        (V3 (maximum xs) (maximum ys) (maximum zs))

primitive :: Shape -> Primitive
primitive shape =
  let tf = foldr ((!*!) . mkTransform) identity shape.transform
      itf = inv44 tf
      mat = material shape.material
   in \ray'@(rayTransform itf -> ray@(Ray o d)) tMin tMax -> case shape.geometry of
        Sphere ->
          let
            dat q =
              HitData
                (getN q)
                (transform point tf (rayAt ray q))
                ( norm (transform point tf (rayAt ray q) ^-^ transform point tf (rayOrigin ray))
                )
                (getUV $ getN q)
            delta =
              ((d `dot` negated o) ^ 2)
                - (quadrance d * (quadrance (negated o) - 1))
            root pm = ((d `dot` negated o) `pm` sqrt delta) / quadrance d
            t = root (-)
            t' = root (+)
            getN x =
              normalize $ transform vector (transpose $ inv44 tf) (rayAt ray x)
            isWithin x = tMin <= x && x <= tMax
            getUV (V3 x y z) = ((atan2 (-z) x + pi) / (2 * pi), acos (-y) / pi)
           in
            if
              | delta < 0 -> Nothing
              | isWithin t -> Just (dat t, mat (dat t) ray)
              | isWithin t' -> Just (dat t', mat (dat t') ray)
              | otherwise -> Nothing
        Plane -> planarIntersection tf ray tMin tMax >>= \dat -> pure (dat, mat dat ray)
        Quad ->
          planarIntersection tf ray tMin tMax >>= \dat ->
            let V3 u _ v = rayAt ray dat.param
                dat' = dat {coords = (u + 0.5, v + 0.5)}
             in guard (all (\x -> x >= -0.5 && x <= 0.5) [u, v])
                  >> pure (dat', mat dat' ray)
        Circle ->
          planarIntersection tf ray tMin tMax >>= \dat ->
            let p@(V3 u _ v) = rayAt ray dat.param
                dat' = dat {coords = ((u + 1) / 2, (v + 1) / 2)}
             in guard (norm p <= 1)
                  >> pure (dat', mat dat' ray)
        Triangle (V3 v1@(V3 ax ay az) v2@(V3 bx by bz) v3@(V3 cx cy cz)) ->
          let (V3 ox oy oz) = o
              (V3 dx dy dz) = d
              a =
                V3
                  (V3 (ax - bx) (ax - cx) dx)
                  (V3 (ay - by) (ay - cy) dy)
                  (V3 (az - bz) (az - cz) dz)
              b = V3 (V1 $ ax - ox) (V1 $ ay - oy) (V1 $ az - oz)
              (V3 (V1 β) (V1 γ) (V1 t)) = inv33 a !*! b
              dat = HitData ((v2 ^-^ v1) `cross` (v3 - v1)) (rayAt ray t) t (β, γ)
           in if
                | det33 a == 0 -> Nothing
                | t <= tMin || t >= tMax -> Nothing
                | β >= 0 && γ >= 0 && β + γ <= 1 -> Just (dat, mat dat ray')
                | otherwise -> Nothing
 where
  planarIntersection tf ray@(Ray o d) tMin tMax =
    let denom = V3 0 1 0 `dot` d
        t = (V3 0 1 0 `dot` (V3 0 0 0 ^-^ o)) / denom
        getN = normalize $ transform point (transpose $ inv44 tf) (V3 0 1 0)
     in if
          | denom == 0 -> Nothing
          | t < tMin || t > tMax -> Nothing
          | otherwise ->
              Just $
                HitData
                  getN
                  (transform point tf (rayAt ray t))
                  t
                  (prettyError "Tried to evaluate uv for infinite surface")

triangulate :: Data.Wavefront.Object -> [Geometry]
triangulate (Data.Wavefront.Object n vs ns uvs fs) =
  map
    ( \[(FaceRef vi1 _ _), (FaceRef vi2 _ _), (FaceRef vi3 _ _)] ->
        traceShow (V3 (vs !! (vi1 - 1)) (vs !! (vi2 - 1)) (vs !! (vi3 - 1))) $
          Triangle $
            V3 (vs !! (vi1 - 1)) (vs !! (vi2 - 1)) (vs !! (vi3 - 1))
    )
    fs

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
-- primitive (Plane (material -> m) tfs) =
--   \(rayTransform itf -> ray@(Ray o ( d))) tMin tMax ->
--     let denom = (V3 0 1 0) `dot` d
--         t = ((V3 0 1 0) `dot` ((V3 0 0 0) ^-^ o)) / denom
--         getN = normalize $ transform point (transpose $ inv44 tf) (V3 0 1 0)
--       in if
--           | denom == 0 -> Nothing
--           | not (t >= tMin && t <= tMax) -> Nothing
--           | otherwise ->
--               Just $
--                 (HitData getN (transform point tf (rayAt ray t)) t $ prettyError "Tried to evaluate uv for infinite surface", m (HitData getN (transform point tf (rayAt ray t)) t $ prettyError "Tried to evaluate uv for infinite surface") ray)
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
