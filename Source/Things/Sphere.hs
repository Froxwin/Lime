module Things.Sphere where

import           Materials                      ( Material )
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

sphere :: Vector -> Double -> Material -> Thing
sphere c r m ray@(Ray a d) tMin tMax
  | delta < 0 = Nothing
  | otherwise = if t' < t
    then (if isWithin t' then Just (normal t', point t', m) else Nothing)
    else (if isWithin t then Just (normal t, point t, m) else Nothing)
 where
  delta =
    ((d `dot` (a `vsub` c)) ^ 2)
      - (magnitude d ^ 2)
      * ((magnitude (a `vsub` c) ^ 2) - r ^ 2)
  root pm = ((-d `dot` (a `vsub` c)) `pm` sqrt delta) / magnitude d ^ 2
  t     = root (+)
  t'    = root (-)
  point = rayAt ray
  normal x = normalize (point x `vsub` c)
  isWithin x = x >= tMin && x <= tMax
