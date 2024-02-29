{-# LANGUAGE DeriveGeneric #-}

module Things.Types where

import           Data.Yaml                      ( FromJSON )
import           GHC.Generics                   ( Generic )
import           Ray                            ( Ray )
import           Vector                         ( Vector )

type Thing = Ray -> Double -> Double -> Maybe (Vector, Vector)

data WorldObject =
    Sphere
      { center :: Vector
      , radius :: Double
      }
  | Cube
      { center :: Vector,
        side :: Double
      }
  deriving (Show, Generic)

instance FromJSON WorldObject
