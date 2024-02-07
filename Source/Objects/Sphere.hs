module Objects.Sphere where

import           Camera                         ( Object )
import           Ray                            ( Ray(Ray)
                                                , rayAt
                                                )
import           Vector                         ( (<->)
                                                , Vector
                                                , dot
                                                , magnitude
                                                , unitVector
                                                )

sphere :: Vector -> Double -> Object
sphere center radius ray tMin tMax
  | delta < 0 = Nothing
  | otherwise = if t' < t
    then (if isWithin t' then Just (normal t', point t') else Nothing)
    else (if isWithin t then Just (normal t, point t) else Nothing)
 where
  delta =
    ((d `dot` (a <-> center)) ^ 2)
      - (magnitude d ^ 2)
      * ((magnitude (a <-> center) ^ 2) - radius ^ 2)
  root pm = ((-d `dot` (a <-> center)) `pm` sqrt delta) / magnitude d ^ 2
  t         = root (+)
  t'        = root (-)
  (Ray a d) = ray
  point     = rayAt (Ray a d)
  normal x = unitVector (point x <-> center)
  isWithin x = x >= tMin && x <= tMax
