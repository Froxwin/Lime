module Ray where

import           Vector                         ( Vector((<+>), (<.>))
                                                , Vector3
                                                )

data Ray = Ray
  { origin    :: Vector3
  , direction :: Vector3
  }
  deriving Show

rayAt :: Ray -> Double -> Vector3
rayAt r t = origin r <+> (t <.> direction r)
