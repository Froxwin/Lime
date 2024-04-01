module Ray where

import Vector (Vector, vadd, vmul)

-- | Represents a light ray
data Ray = Ray
  { rayOrigin    :: Vector -- ^ Point from where the ray is emanating
  , rayDirection :: Vector -- ^ direction in which ray is going
  }

-- | Gives the point on ray at given parameter
rayAt :: Ray -> Double -> Vector
rayAt (Ray o d) t = o `vadd` (t `vmul` d)
