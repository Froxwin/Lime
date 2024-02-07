module Ray where

import           Vector                         ( (<+>)
                                                , (<.>)
                                                , Vector
                                                )

data Ray = Ray
  { origin    :: Vector
  , direction :: Vector
  }

rayAt :: Ray -> Double -> Vector
rayAt r t = origin r <+> (t <.> direction r)
