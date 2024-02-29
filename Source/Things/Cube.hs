module Things.Cube where

import           Ray                            ( Ray(Ray)
                                                , rayAt
                                                )
import           Things.Types                   ( Thing )
import           Vector                         ( Vector
                                                , dot
                                                , magnitude
                                                , normalize
                                                , vsub
                                                )

sphere :: Vector -> Double -> Thing
sphere center radius ray tMin tMax
  | delta < 0 = Nothing
  | otherwise = if t' < t
    then (if isWithin t' then Just (normal t', point t') else Nothing)
    else (if isWithin t then Just (normal t, point t) else Nothing)
 where
  delta =
    ((d `dot` (a `vsub` center)) ^ 2)
      - (magnitude d ^ 2)
      * ((magnitude (a `vsub` center) ^ 2) - radius ^ 2)
  root pm = ((-d `dot` (a `vsub` center)) `pm` sqrt delta) / magnitude d ^ 2
  t         = root (+)
  t'        = root (-)
  (Ray a d) = ray
  point     = rayAt (Ray a d)
  normal x = normalize (point x `vsub` center)
  isWithin x = x >= tMin && x <= tMax
