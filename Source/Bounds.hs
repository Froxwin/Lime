module Bounds where

import           Color
import           Data.Tuple
import           Ray
import           Vector

class Box a where
    xbx :: a -> ( Double, Double )
    ybx :: a -> ( Double, Double )
    zbx :: a -> ( Double, Double )

data AABB = AABB { a :: Vector, b :: Vector }
          | AABBbox { box0 :: AABB, box1 :: AABB }
          deriving (Show)

-- instance Box AABBbox where
--   xbx (AABBbox b0 b1) = (fst $ xbx b0, snd $ xbx b1)
--   ybx (AABBbox b0 b1) = (fst $ ybx b0, snd $ ybx b1)
--   zbx (AABBbox b0 b1) = (fst $ zbx b0, snd $ zbx b1)

instance Box AABB where
  xbx (AABB    a  b ) = (min (vx a) (vx b), max (vx a) (vx b))
  xbx (AABBbox b0 b1) = (fst $ xbx b0, snd $ xbx b1)

  ybx (AABB    a  b ) = (min (vy a) (vy b), max (vy a) (vy b))
  ybx (AABBbox b0 b1) = (fst $ ybx b0, snd $ ybx b1)

  zbx (AABB    a  b ) = (min (vz a) (vz b), max (vz a) (vz b))
  zbx (AABBbox b0 b1) = (fst $ zbx b0, snd $ zbx b1)

hit aabb ray@(Ray o d) rayT@(mi, ma) = map
  (\i ->
    let invD = 1 / comp i d
        orig = comp i o
        t0   = (fst (axis i) - orig) * invD
        t1   = (snd (axis i) - orig) * invD
        q0   = if invD < 0 then t1 else t0
        q1   = if invD < 0 then t0 else t1
        w0   = max q0 mi
        w1   = min q1 ma
    in  if w1 <= w0 then Nothing else Just (w0, w1)
  )
  [0 .. 3]
 where
  comp n | n == 1    = vy
         | n == 2    = vz
         | otherwise = vx

  axis n | n == 1    = ybx aabb
         | n == 2    = zbx aabb
         | otherwise = xbx aabb
