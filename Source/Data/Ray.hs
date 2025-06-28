module Data.Ray where

import Linear.Matrix    ((!*!))
import Linear.Transform (Transform, identity, inv44, mkTransform, point,
                         transform, vector)
import Linear.V3        (V3)
import Linear.Vector    (Additive ((^+^)), (*^))

data Ray = Ray
  { rayOrigin :: V3 Double
  , rayDirection :: V3 Double
  }
  deriving (Show, Eq)

rayAt :: Ray -> Double -> V3 Double
rayAt (Ray o d) t = o ^+^ (t *^ d)
{-# INLINE rayAt #-}

-- | Performs the inverse of a list of transformations on a ray
rayTransform :: [Transform] -> Ray -> Ray
rayTransform ts (Ray o d) = Ray (transform point tf o) (transform vector tf d)
 where
  tf = foldr ((!*!) . inv44 . mkTransform) identity ts
