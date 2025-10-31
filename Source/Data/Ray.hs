module Data.Ray where

import Linear.Matrix ((!*!), M44)
import Linear.Metric
import Linear.Transform
  ( Transform
  , identity
  , inv44
  , mkTransform
  , point
  , transform
  , vector
  )
import Linear.V3 (V3)
import Linear.Vector (Additive ((^+^)), (*^))

data Ray = Ray
  { rayOrigin :: V3 Double
  , rayDirection :: (V3 Double)
  }
  deriving (Show, Eq)

rayAt :: Ray -> Double -> V3 Double
rayAt (Ray o (d)) t = o ^+^ (t *^ d)
{-# INLINE rayAt #-}

-- | Performs the inverse of a list of transformations on a ray
rayTransform :: M44 Double -> Ray -> Ray
rayTransform itf (Ray o (d)) = Ray (transform point itf o) (transform vector itf d)
