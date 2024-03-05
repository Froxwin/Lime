{-# LANGUAGE DeriveGeneric #-}

module Things.Types where

import           Data.Yaml                      ( FromJSON )
import           GHC.Generics                   ( Generic )
import Ray ( Ray(Ray) )
import Vector ( Vector, vadd )

type Thing = Ray -> Double -> Double -> Maybe (Vector, Vector, Material)

data WorldObject =
    Sphere
      { center :: Vector
      , radius :: Double
      , mat    :: Color
      }
  | Cube
      { center :: Vector,
        side :: Double
        , mat :: Color
      }
  deriving (Show, Generic)

instance FromJSON WorldObject

type Material = Vector -> Vector -> Vector -> (Color, Ray)

lambertian :: Color -> Material
lambertian c r n p = (c, Ray p (r `vadd` n))

-- | Represents a rgb color type
data Color = Color
  { red   :: Double -- ^ The red component of a color
  , green :: Double -- ^ The green component of a color
  , blue  :: Double -- ^ The blue component of a color
  }
  deriving (Eq, Generic)

instance Show Color where
  show :: Color -> String
  show (Color r g b) = concat $ (flip $ zipWith (++)) [" ", " ", ""] $ map
    (show . round . (* 255))
    [r, g, b]

instance FromJSON Color
