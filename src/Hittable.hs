module Hittable where

import           Ray                            ( Ray(Ray, direction)
                                                , rayAt
                                                )
import           Vector                         ( Vector((<+>), (<->), dot)
                                                , Vector3(Vector3)
                                                , magnitude
                                                , unitVector
                                                , neg
                                                )

data HitRecord = HitRecord
  { point     :: Vector3
  , normal    :: Vector3
  , t         :: Double
  }

faceNormal record r outwardNormal = if (direction r `dot` outwardNormal) < 0 then outwardNormal else neg outwardNormal

class Hittable a where
  hit :: a -> Ray -> Double -> Double -> Maybe HitRecord

data Sphere = Sphere
  { center :: Vector3
  , radius :: Double
  }

instance Hittable Sphere where
  hit (Sphere c r) (Ray a d) tMin tMax
    | delta < 0 = Nothing
    | otherwise = if t <= tMin || t >= tMax
      then
        (if t' <= tMin || t' >= tMax
          then Nothing
          else
            let p = rayAt (Ray a d) t'
            in  Just (HitRecord p (unitVector (p <+> Vector3 0 0 1)) t')
        )
      else
        let p = rayAt (Ray a d) t
        in  Just (HitRecord p (unitVector (p <+> Vector3 0 0 1)) t)
   where
    delta =
      ((d `dot` (a <-> c)) ^ 2)
        - (magnitude d ^ 2)
        * ((magnitude (a <-> c) ^ 2) - r ^ 2)
    t  = ((-d `dot` (a <-> c)) + sqrt delta) / magnitude d ^ 2
    t' = ((-d `dot` (a <-> c)) + sqrt delta) / magnitude d ^ 2
