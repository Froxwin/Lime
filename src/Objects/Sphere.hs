module Objects.Sphere where

import           Objects.Hittable               ( HitRecord(HitRecord)
                                                , Hittable(..)
                                                )
import           Ray                            ( Ray(Ray)
                                                , rayAt
                                                )
import           Vector                         ( Vector((<+>), (<->), dot)
                                                , Vector3(Vector3)
                                                , magnitude
                                                , unitVector
                                                )

data Sphere = Sphere
  { center :: Vector3
  , radius :: Double
  }

instance Hittable Sphere where
  hit :: Sphere -> Ray -> Double -> Double -> Maybe HitRecord
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
