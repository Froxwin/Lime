{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Parser where

import           Color        (Color)
import           Data.Yaml    (FromJSON)
import           GHC.Generics (Generic)
import           Materials    (dielectric, lambertian, metal)
import           Things       (Thing, sphere)
import           Vector       (Vector)

data WorldMaterial =
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

instance FromJSON WorldMaterial

data WorldObject = Sphere
  { center   :: Vector
  , radius   :: Double
  , material :: WorldMaterial
  }
  deriving (Show, Generic)

instance FromJSON WorldObject

parseWorldObject :: WorldObject -> Thing
parseWorldObject (Sphere c r (Lambertian q)) = sphere c r (lambertian q)
parseWorldObject (Sphere c r (Metal q f   )) = sphere c r (metal q f)
parseWorldObject (Sphere c r (Dielectric i)) = sphere c r (dielectric i)
