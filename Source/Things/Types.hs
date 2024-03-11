{-# LANGUAGE DeriveGeneric #-}

module Things.Types where

import           Color                          ( Color )
import           Data.Yaml                      ( FromJSON )
import           GHC.Generics                   ( Generic )
import           Materials                      ( Material )
import           Ray                            ( Ray )
import           Vector                         ( Vector )

type Thing = Ray -> Double -> Double -> Maybe (Vector, Vector, Material)

data The =
    Lambertian
      { color :: Color
      }
  | Metal
      { color :: Color
      , fuzz  :: Double
      }
  | Dielectric
      { ior :: Double
      }
  deriving (Show, Generic, Eq)

instance FromJSON The

data WorldObject = Sphere
  { center   :: Vector
  , radius   :: Double
  , material :: The
  }
  deriving (Show, Generic)

instance FromJSON WorldObject
