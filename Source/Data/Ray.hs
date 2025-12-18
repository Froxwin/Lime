module Data.Ray where

import Linear.Matrix
import Linear.Transform
import Linear.V3
import Linear.Vector

data Ray = Ray
  { rayOrigin :: {-# UNPACK #-} !(V3 Float)
  , rayDirection :: {-# UNPACK #-} !(V3 Float)
  }
  deriving Show

rayAt :: Ray -> Float -> V3 Float
rayAt (Ray o d) t = o ^+^ (t *^ d)
{-# INLINE rayAt #-}

-- | Performs the inverse of a list of transformations on a ray
rayTransform :: M44 Float -> Ray -> Ray
rayTransform itf (Ray o d) = Ray (transform point itf o) (transform vector itf d)
{-# INLINE rayTransform #-}

-- | Performs the inverse of a list of transformations on a ray
-- rayTransform :: M44 -> Ray -> Ray
-- rayTransform itf (Ray o d) = Ray (transform point itf o) (transform vector itf d)
-- {-# INLINE rayTransform #-}

-- rayTransform :: M44 Double -> Ray -> Ray
-- rayTransform itf (Ray o d) = Ray (transformPoint itf o) (transformVector itf d)
-- {-# INLINE rayTransform #-}

-- {-# INLINE transformPoint #-}
-- transformPoint :: M44 -> V3 -> V3
-- transformPoint
--   ( V4
--       (V4 m00 m01 m02 m03)
--       (V4 m10 m11 m12 m13)
--       (V4 m20 m21 m22 m23)
--       _ -- ignore last row for points? Or include if homogeneous
--     )
--   (V3 x y z) =
--     V3
--       (m00 * x + m01 * y + m02 * z + m03)
--       (m10 * x + m11 * y + m12 * z + m13)
--       (m20 * x + m21 * y + m22 * z + m23)

-- {-# INLINE transformVector #-}
-- transformVector :: M44 -> V3 -> V3
-- transformVector
--   ( V4
--       (V4 m00 m01 m02 _)
--       (V4 m10 m11 m12 _)
--       (V4 m20 m21 m22 _)
--       _
--     )
--   (V3 x y z) =
--     V3
--       (m00 * x + m01 * y + m02 * z)
--       (m10 * x + m11 * y + m12 * z)
--       (m20 * x + m21 * y + m22 * z)
