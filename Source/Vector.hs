module Vector where

import Data.Yaml    (FromJSON (parseJSON), Parser, Value)
import GHC.Generics (Generic)

data Vec3 = Vec3
  { vx :: Double
  , vy :: Double
  , vz :: Double
  }
  deriving (Eq, Generic)

vadd, vsub :: Vec3 -> Vec3 -> Vec3
vadd (Vec3 x y z) (Vec3 x' y' z') = Vec3 (x + x') (y + y') (z + z')
vsub (Vec3 x y z) (Vec3 x' y' z') = Vec3 (x - x') (y - y') (z - z')

vmul, vdiv :: Double -> Vec3 -> Vec3
vmul t (Vec3 x y z) = Vec3 (t * x) (t * y) (t * z)
vdiv t (Vec3 x y z) = Vec3 (1 / t * x) (1 / t * y) (1 / t * z)

dot :: Vec3 -> Vec3 -> Double
dot (Vec3 x y z) (Vec3 x' y' z') = x * x' + y * y' + z * z'

cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 x y z) (Vec3 x' y' z') =
  Vec3 (y * z' - z * y') (z * x' - x * z') (x * y' - y * x')

vneg :: Vec3 -> Vec3
vneg (Vec3 x y z) = Vec3 (-x) (-y) (-z)

magnitude, magnitudeSquare :: Vec3 -> Double
magnitudeSquare v = v `dot` v
magnitude = sqrt . magnitudeSquare

normalize :: Vec3 -> Vec3
normalize v = magnitude v `vdiv` v

-- | Reflects a vector with respect to the given surface normal
--
-- > reflect :: Incident Vector -> Surface Normal -> Reflected Vector
reflect :: Vec3 -> Vec3 -> Vec3
reflect v n = v `vsub` ((2 * (v `dot` n)) `vmul` n)

-- | Refracts a vector with respect to the given surface normal and relative
-- refractive index of the interface
--
-- > refract :: Incident Vector -> Surface Normal -> Relative IOR -> Refracted Vector
refract :: Vec3 -> Vec3 -> Double -> Vec3
refract v n mu = rperp `vadd` rpara
 where
  cosine = min (vneg (normalize v) `dot` normalize n) 1
  rperp  = mu `vmul` (normalize v `vadd` (cosine `vmul` normalize n))
  rpara  = (-sqrt (abs (1 - magnitudeSquare rperp))) `vmul` normalize n

-- | Compare vectors with respect to their magnitudes
instance Ord Vec3 where
  (<=) :: Vec3 -> Vec3 -> Bool
  a <= b = magnitudeSquare a <= magnitudeSquare b

instance Show Vec3 where
  show :: Vec3 -> String
  show (Vec3 x y z) = show [x, y, z]

instance FromJSON Vec3 where
  parseJSON :: Value -> Parser Vec3
  parseJSON v = do
    [x, y, z] <- parseJSON v
    return $ Vec3 x y z
