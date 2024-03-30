{-# OPTIONS_GHC -Wno-partial-fields #-}

module Bounds where

import           Ray
import           Vector

class Box a where
    xbx :: a -> ( Double, Double )
    ybx :: a -> ( Double, Double )
    zbx :: a -> ( Double, Double )

data AABB = AABB { a :: Vector, b :: Vector }
          | AABBq { xs :: (Double, Double), ys :: (Double, Double), zs :: (Double, Double) }
          | AABBbox { box0 :: AABB, box1 :: AABB }
          deriving (Show)

pad :: Box p => p -> AABB
pad aabb = AABBq nxbx nybx nzbx
 where
  delta      = 0.01
  padding    = delta / 2
  x@(x1, x2) = xbx aabb
  y@(y1, y2) = ybx aabb
  z@(z1, z2) = zbx aabb
  nxbx       = if x2 - x1 >= delta then x else (x1 - padding, x2 + padding)
  nybx       = if y2 - y1 >= delta then y else (y1 - padding, y2 + padding)
  nzbx       = if z2 - z1 >= delta then z else (z1 - padding, z2 + padding)

instance Box AABB where
  xbx (AABB    aw bw) = (min (vx aw) (vx bw), max (vx aw) (vx bw))
  xbx (AABBbox b0 b1) = (fst $ xbx b0, snd $ xbx b1)
  xbx (AABBq x _ _  ) = x

  ybx (AABB    aw bw) = (min (vy aw) (vy bw), max (vy aw) (vy bw))
  ybx (AABBbox b0 b1) = (fst $ ybx b0, snd $ ybx b1)
  ybx (AABBq _ y _  ) = y

  zbx (AABB    aw bw) = (min (vz aw) (vz bw), max (vz aw) (vz bw))
  zbx (AABBbox b0 b1) = (fst $ zbx b0, snd $ zbx b1)
  zbx (AABBq _ _ z  ) = z

hit :: Box p => p -> Ray -> (Double, Double) -> [Maybe (Double, Double)]
hit aabb (Ray o d) (mi, ma) = map
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
