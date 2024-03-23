module Vector where

import           Data.Yaml                      ( FromJSON(parseJSON)
                                                , Parser
                                                , Value
                                                )
import           GHC.Generics                   ( Generic )

data Vector = Vector
  { vx :: Double
  , vy :: Double
  , vz :: Double
  }
  deriving (Eq, Generic)

vadd, vsub :: Vector -> Vector -> Vector
vadd (Vector x y z) (Vector x' y' z') = Vector (x + x') (y + y') (z + z')
vsub (Vector x y z) (Vector x' y' z') = Vector (x - x') (y - y') (z - z')

vmul, vdiv :: Double -> Vector -> Vector
vmul t (Vector x y z) = Vector (t * x) (t * y) (t * z)
vdiv t (Vector x y z) = Vector (1 / t * x) (1 / t * y) (1 / t * z)

dot :: Vector -> Vector -> Double
dot (Vector x y z) (Vector x' y' z') = x * x' + y * y' + z * z'

cross :: Vector -> Vector -> Vector
cross (Vector x y z) (Vector x' y' z') =
  Vector (y * z' - z * y') (z * x' - x * z') (x * y' - y * x')

vneg :: Vector -> Vector
vneg (Vector x y z) = Vector (-x) (-y) (-z)

magnitude, magnitudeSq :: Vector -> Double
magnitudeSq v = v `dot` v
magnitude = sqrt . magnitudeSq

normalize :: Vector -> Vector
normalize v = magnitude v `vdiv` v

-- | Reflects a vector with respect to the given surface normal
--
-- > reflect :: Incident Vector -> Surface Normal -> Reflected Vector
reflect :: Vector -> Vector -> Vector
reflect v n = v `vsub` ((2 * (v `dot` n)) `vmul` n)

-- | Refracts a vector with respect to the given surface normal and relative
-- refractive index of the interface
--
-- > refract :: Incident Vector -> Surface Normal -> Relative IOR -> Refracted Vector
refract :: Vector -> Vector -> Double -> Vector
refract v n mu = rperp `vadd` rpara
 where
  cosine = vneg v `dot` n
  rperp  = mu `vmul` (v `vadd` (cosine `vmul` n))
  rpara  = (-sqrt (abs (1 - magnitudeSq v))) `vmul` n

-- | Compare vectors with respect to their magnitudes
instance Ord Vector where
  (<=) :: Vector -> Vector -> Bool
  a <= b = magnitudeSq a <= magnitudeSq b

instance Show Vector where
  show :: Vector -> String
  show (Vector x y z) = show [x, y, z]

instance FromJSON Vector where
  parseJSON :: Value -> Parser Vector
  parseJSON v = do
    [x, y, z] <- parseJSON v
    return $ Vector x y z
