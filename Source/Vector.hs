module Vector where

class Vector a where
    (<+>) :: a -> a -> a
    (<->) :: a -> a -> a
    (<.>) :: Double -> a -> a
    (</>) :: a -> Double -> a
    dot :: a -> a -> Double
    cross :: a -> a -> a
    neg :: a -> a

magnitude :: Vector a => a -> Double
magnitude v = sqrt (v `dot` v)

unitVector :: Vector a => a -> a
unitVector v = v </> magnitude v

data Vector3 = Vector3
  { vx :: Double
  , vy :: Double
  , vz :: Double
  }
  deriving Eq

instance Vector Vector3 where
  (<+>) :: Vector3 -> Vector3 -> Vector3
  (<+>) (Vector3 x y z) (Vector3 x' y' z') =
    Vector3 (x + x') (y + y') (z + z')
  (<->) :: Vector3 -> Vector3 -> Vector3
  (<->) (Vector3 x y z) (Vector3 x' y' z') =
    Vector3 (x - x') (y - y') (z - z')
  (<.>) :: Double -> Vector3 -> Vector3
  (<.>) t (Vector3 x y z) = Vector3 (t * x) (t * y) (t * z)
  (</>) :: Vector3 -> Double -> Vector3
  (</>) (Vector3 x y z) t = Vector3 (1 / t * x) (1 / t * y) (1 / t * z)
  dot :: Vector3 -> Vector3 -> Double
  dot (Vector3 x y z) (Vector3 x' y' z') = x * x' + y * y' + z * z'
  cross :: Vector3 -> Vector3 -> Vector3
  cross (Vector3 x y z) (Vector3 x' y' z') =
    Vector3 (y * z' - z * y') (z * x' - x * z') (x * y' - y * x')
  neg :: Vector3 -> Vector3
  neg (Vector3 x y z) = Vector3 (-x) (-y) (-z)

instance Show Vector3 where
  show :: Vector3 -> String
  show (Vector3 x y z) =
    concat ["(", show x, ", ", show y, ", ", show z, ")"]

instance Ord Vector3 where
  (<=) :: Vector3 -> Vector3 -> Bool
  a <= b = magnitude a <= magnitude b
