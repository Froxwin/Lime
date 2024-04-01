module Color where

import Data.Yaml    (FromJSON (parseJSON), Parser, Value)
import GHC.Generics (Generic)

-- | Represents a rgb color type with fields representing respective color
--   channels
data Color = Color
  { red   :: Double
  , green :: Double
  , blue  :: Double
  }
  deriving (Eq, Generic)

-- | Converts a color to string while also converting the color channels to 8bit
--   rgb
instance Show Color where
  show :: Color -> String
  show (Color r g b) = show $ map (round . (* 255)) [r, g, b]

instance FromJSON Color where
  parseJSON :: Value -> Parser Color
  parseJSON v = do
    [x, y, z] <- parseJSON v
    return $ Color (x / 255) (y / 255) (z / 255)

-- | Convert color from linear space to gamma space with γ = 1/2
correctGamma :: Color -> Color
correctGamma (Color r g b) = Color (sqrt r) (sqrt g) (sqrt b)

-- | Adds two colors by adding their respective rgb components
addColor :: Color -> Color -> Color
addColor (Color r g b) (Color r' g' b') = Color (r + r') (g + g') (b + b')

-- | Scales a color by scaling all its color channels
scaleColor :: Double -> Color -> Color
scaleColor t (Color r g b) = Color (t * r) (t * g) (t * b)

-- | Scales a color by multiplying its color channels with the given color
scaleColor' :: Color -> Color -> Color
scaleColor' (Color r g b) (Color r' g' b') = Color (r * r') (g * g') (b * b')
