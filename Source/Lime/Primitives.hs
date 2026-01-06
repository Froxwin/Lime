{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Lime.Primitives where

import Control.Applicative (liftA3)
import Control.DeepSeq (NFData (..), force)
import Control.Lens ((^.), _Unwrapped')
import Control.Monad (guard)
import Data.Aeson.Types
import Data.Color (Color (Color))
import Data.Fixed (mod')
import Data.Function (on)
import Data.Functor.Classes (eq2)
import Data.List (foldl1', minimumBy, sortBy)
import Data.Maybe (catMaybes, fromJust, isJust)
import Data.Ord (comparing)
import Data.Ray
import Data.Wavefront (FaceRef (FaceRef), Object (Object))
import Debug.Trace (traceShow)
import Debug.Trace qualified
import Debug.Trace qualified as Debug
import GHC.Generics
import Lime.Context
import Lime.Internal.Hit
import Lime.Internal.Utils
import Lime.Materials
import Lime.Textures (TextureNode (SolidColor))
import Linear
import Linear.Transform

type Primitive = Ray -> Double -> Double -> Maybe (HitData, Material)

data Geometry
  = Sphere
  | Plane
  | Circle
  | Quad
  | Triangle !(V3 (V3 Double)) !(V3 (V3 Double)) !(V3 (V2 Double))
  deriving (Show, Eq, Generic)

instance FromJSON (V2 Double)

instance FromJSON Geometry where
  parseJSON :: Value -> Parser Geometry
  parseJSON = worldParse

data Shape = Shape
  { geometry :: !Geometry
  , transform :: ![Transform]
  , material :: !MaterialNode
  }
  deriving (Show, Generic, Eq)

data Mesh = Mesh
  { path :: !String
  , transform :: ![Transform]
  , material :: !MaterialNode
  }
  deriving (Show, Eq, Generic)

instance FromJSON Mesh where
  parseJSON :: Value -> Parser Mesh
  parseJSON = worldParse

instance FromJSON Shape where
  parseJSON :: Value -> Parser Shape
  parseJSON = worldParse

instance NFData Geometry

instance NFData Shape

data AABB = AABB
  { lo :: {-# UNPACK #-} !(V3 Double)
  , hi :: {-# UNPACK #-} !(V3 Double)
  }
  deriving (Show, Generic)

data BVH
  = BVHLeaf !Primitive {-# UNPACK #-} !AABB
  | BVHNode {-# UNPACK #-} !AABB !BVH !BVH

instance NFData AABB

instance NFData BVH where
  rnf (BVHLeaf prim box) =
    -- do NOT force prim (it's a function)
    rnf box
  rnf (BVHNode box left right) =
    rnf box `seq` rnf left `seq` rnf right

hsv2rgb (h, s, v) = (r + m, g + m, b + m)
 where
  c = v * s
  h' = h / (pi / 3) -- normalize to [0,6)
  x = c * (1 - abs ((h' `mod'` 2) - 1))
  m = v - c
  sector = floor h' :: Int
  (r, g, b) = case sector `mod` 6 of
    0 -> (c, x, 0)
    1 -> (x, c, 0)
    2 -> (0, c, x)
    3 -> (0, x, c)
    4 -> (x, 0, c)
    _ -> (c, 0, x)

aabbColor :: AABB -> Color Double
aabbColor (AABB (V3 lox loy loz) (V3 hix hiy hiz)) =
  let
    h = abs $ ((hix - lox) + (hiy - loy) + (hiz - loz)) `mod'` (2 * pi)
    (r, g, b) = hsv2rgb (h, 1, 1)
   in
    Color r g b

wireframe :: AABB -> Primitive
wireframe bo@(AABB (V3 lox loy loz) (V3 hix hiy hiz)) ray@(Ray o d) _ _ =
  case catMaybes hits of
    [] -> Nothing
    [h] -> Just h
    hs -> Just $ minimumBy (compare `on` (\(q, _) -> q.param)) hs
 where
  verts = [V3 x y z | x <- [lox, hix], y <- [loy, hiy], z <- [loz, hiz]]
  -- edges = [(a, b) | a <- verts, b <- verts, a /= b]
  diffs (V3 x1 y1 z1) (V3 x2 y2 z2) = length $ filter id [x1 /= x2, y1 /= y2, z1 /= z2]
  edges = [(a, b) | a <- verts, b <- verts, a < b, let d = diffs a b, d == 1 || d == 2]
  hits =
    [ let o' = v2
          d' = (v2 ^-^ v1)
          n = (1 / norm (d' `cross` d)) *^ (d' `cross` d)
          sep = abs $ n `dot` (o ^-^ o')
          en = d `cross` d'
          en1 = d `cross` en
          en2 = d' `cross` en
          p = o ^+^ ((((o' ^-^ o) `dot` en2) / (d `dot` en2)) *^ d)

          kac = (v2 ^-^ v1) `dot` (p ^-^ v1)
          kab = (v2 ^-^ v1) `dot` (v2 ^-^ v1)
          tee = ((o' ^-^ o) `dot` en2) / (d `dot` en2)
          dee = HitData undefined p tee undefined
       in if (sep <= 0.001) && (0 <= kac) && (kac <= kab)
            then
              Just
                ( dee
                , material
                    undefined
                    (Emissive 1 (SolidColor (aabbColor bo)))
                    dee
                    ray
                )
            else Nothing
    | (v1, v2) <- edges
    ]

traverseBVH :: BVH -> Primitive
traverseBVH (BVHLeaf prim box@(AABB lo hi)) = prim
-- \ray@(Ray o d) (realToFrac -> tMin) (realToFrac -> tMax) ->
--   let t1 = (/) <$> (lo ^-^ o) <*> d
--       t2 = (/) <$> (hi ^-^ o) <*> d
--       tmin = maximum $ min <$> t1 <*> t2
--       tmax = minimum $ max <$> t1 <*> t2
--       tmin' = max tMin (tmin - 1e-6)
--       tmax' = min tMax (tmax + 1e-6)
--       hits =
--         catMaybes [prim ray tMin tMax, wireframe box ray tMin tMax]
--    in do
--         guard (tmax' >= tmin')
--         case hits of
--           [] -> Nothing
--           [h] -> Just h
--           hs -> Just $ minimumBy (compare `on` (\(q, _) -> q.param)) hs
traverseBVH (BVHNode box@(AABB lo hi) left right) =
  {-# SCC "slabClosure" #-}
  \ray@(Ray o d) tMin tMax ->
    let t1 = (/) <$> (lo ^-^ o) <*> d
        t2 = (/) <$> (hi ^-^ o) <*> d
        tmin = maximum $ min <$> t1 <*> t2
        tmax = minimum $ max <$> t1 <*> t2
        tmin' = max tMin (tmin - 1e-6)
        tmax' = min tMax (tmax + 1e-6)
        hits =
          catMaybes
            [ traverseBVH left ray tMin tMax
            , traverseBVH right ray tMin tMax
            -- , wireframe box ray tMin tMax
            ]
     in do
          guard (tmax' >= tmin')
          case hits of
            [] -> Nothing
            [h] -> Just h
            hs -> Just $ minimumBy (compare `on` (\(q, _) -> q.param)) hs

constructBVH :: RenderCtx -> [Shape] -> BVH
constructBVH _ [] = error "constructBVH: empty scene"
constructBVH ctx shapes = buildBVH ctx $ map (\s -> (shapeBounds s, s)) shapes

buildBVH :: RenderCtx -> [(AABB, Shape)] -> BVH
buildBVH ctx items =
  case items of
    [] -> error "buildBVH: empty"
    [(bbox, s)] ->
      BVHLeaf (primitive ctx s) bbox
    [(b1, s1), (b2, s2)] ->
      let nodeB = unionAABB b1 b2
       in BVHNode
            nodeB
            (BVHLeaf (primitive ctx s1) b1)
            (BVHLeaf (primitive ctx s2) b2)
    _ ->
      let (AABB bmin bmax) = foldl1 unionAABB (map fst items)
          -- choose longest axis
          ext = bmax ^-^ bmin
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
          leftBVH = buildBVH ctx leftItems
          rightBVH = buildBVH ctx rightItems
          leftB = bboxOfBVH leftBVH
          rightB = bboxOfBVH rightBVH
          (AABB nodeMin nodeMax) = unionAABB leftB rightB
       in BVHNode (AABB bmin bmax) leftBVH rightBVH

centroid :: AABB -> V3 Double
centroid (AABB lo hi) = (lo ^+^ hi) ^/ 2

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
  Quad -> AABB (V3 (-0.5) (-1e-2) (-0.5)) (V3 0.5 1e-2 0.5)
  Circle -> AABB (V3 (-1) (-1e-2) (-1)) (V3 1 1e-2 1)
  Plane -> AABB (V3 (-1e6) (-1e-2) (-1e6)) (V3 1e6 1e-2 1e6)
  Triangle (V3 v1 v2 v3) _ _ ->
    let lo = liftA3 (\x1 x2 x3 -> minimum [x1, x2, x3]) v1 v2 v3
        hi = liftA3 (\x1 x2 x3 -> maximum [x1, x2, x3]) v1 v2 v3
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

primitive :: RenderCtx -> Shape -> Primitive
primitive ctx shape =
  let tf = force $ foldr ((!*!) . mkTransform) identity shape.transform
      itf = force $ inv44 tf
      mat = force $ material ctx shape.material
   in {-# SCC "primitiveClosure" #-}
      \ray'@(rayTransform itf -> ray@(Ray o d)) tMin tMax -> case shape.geometry of
        Sphere ->
          let dat q =
                HitData
                  (getN q)
                  (transform point tf (rayAt ray q))
                  (norm (transform point tf (rayAt ray q) ^-^ transform point tf (rayOrigin ray)))
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
           in if
                | delta < 0 -> Nothing
                | isWithin t -> Just (dat t, mat (dat t) ray')
                | isWithin t' -> Just (dat t', mat (dat t') ray')
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
        Triangle
          (V3 (V3 ax ay az) (V3 bx by bz) (V3 cx cy cz))
          (V3 n1 n2 n3)
          (V3 t1 t2 t3) ->
            let (V3 ox oy oz) = o
                (V3 dx dy dz) = d
                a =
                  V3
                    (V3 (ax - bx) (ax - cx) dx)
                    (V3 (ay - by) (ay - cy) dy)
                    (V3 (az - bz) (az - cz) dz)
                b = V3 (V1 $ ax - ox) (V1 $ ay - oy) (V1 $ az - oz)
                (V3 (V1 β) (V1 γ) (V1 t)) = inv33 a !*! b
                α = 1 - β - γ
                n0 = normalize $ (α *^ n1) ^+^ (β *^ n2) ^+^ (γ *^ n3)
                getN n = normalize $ transform vector (transpose $ inv44 tf) n
                en = getN n0
                uv = (α *^ t1) ^+^ (β *^ t2) ^+^ (γ *^ t3)
                dat =
                  HitData
                    (if en `dot` (transform vector tf d) > 0 then negated en else en) -- normal
                    (transform point tf (rayAt ray t))
                    t
                    ((\(V2 q1 q2) -> (q1, q2)) uv)
             in if
                  | det33 a == 0 -> Nothing
                  | t <= tMin || t >= tMax -> Nothing
                  | β >= 0 && γ >= 0 && β + γ <= 1 -> Just (dat, mat dat ray')
                  | otherwise -> Nothing
 where
  planarIntersection tf ray@(Ray o d) tMin tMax =
    let denom = V3 0 1 0 `dot` d
        t = (V3 0 1 0 `dot` (V3 0 0 0 ^-^ o)) / denom
        en = normalize $ transform vector (transpose $ inv44 tf) (V3 0 1 0)
     in if
          | abs denom <= 1e-6 -> Nothing
          | t < tMin || t > tMax -> Nothing
          | otherwise ->
              Just $
                HitData
                  (if en `dot` (transform vector tf d) > 0 then negated en else en)
                  (transform point tf (rayAt ray t))
                  t
                  (prettyError "Tried to evaluate uv for infinite surface")

triangulate :: Data.Wavefront.Object -> [Geometry]
triangulate (Data.Wavefront.Object n vs ns ts fs) =
  map
    ( \case
        [ FaceRef vi1 (Just ti1) (Just ni1)
          , FaceRef vi2 (Just ti2) (Just ni2)
          , FaceRef vi3 (Just ti3) (Just ni3)
          ] ->
            Triangle
              (V3 (vs !! (vi1 - 1)) (vs !! (vi2 - 1)) (vs !! (vi3 - 1)))
              (V3 (ns !! (ni1 - 1)) (ns !! (ni2 - 1)) (ns !! (ni3 - 1)))
              (V3 (ts !! (ti1 - 1)) (ts !! (ti2 - 1)) (ts !! (ti3 - 1)))
        _ -> error "Invalid mesh (possibly not triangulated)"
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
