module Ray where

import Vector (Vec3, vadd, vmul)

-- | Represents a light ray
data Ray = Ray
  { rayOrigin :: Vec3
  -- ^ Point from where the ray is emanating
  , rayDirection :: Vec3
  -- ^ direction in which ray is going
  }

-- | Gives the point on ray at given parameter
rayAt :: Ray -> Double -> Vec3
rayAt (Ray o d) t = o `vadd` (t `vmul` d)
