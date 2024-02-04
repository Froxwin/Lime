module Objects.Hittable where

import           Ray                            ( Ray(direction) )
import           Vector                         ( Vector(dot, neg)
                                                , Vector3
                                                )

data HitRecord = HitRecord
  { hitPoint  :: Vector3
  , hitNormal :: Vector3
  , hitParam  :: Double
  }

faceNormal record r outwardNormal =
  if (direction r `dot` outwardNormal) < 0
    then outwardNormal
    else neg outwardNormal

class Hittable a where
  hit :: a -> Ray -> Double -> Double -> Maybe HitRecord
