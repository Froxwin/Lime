{-# LANGUAGE DeriveGeneric #-}

module Color where

import           Data.Yaml                      ( FromJSON(parseJSON)
                                                , Parser
                                                , Value
                                                )
import           GHC.Generics                   ( Generic )

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

instance FromJSON Color where
  parseJSON :: Value -> Parser Color
  parseJSON v = do
    [x, y, z] <- parseJSON v
    return $ Color (x / 255) (y / 255) (z / 255)

correctGamma :: Color -> Color
correctGamma (Color r g b) = Color (sqrt r) (sqrt g) (sqrt b)

-- | Adds two colors by adding their respective rgb components
addColor :: Color -> Color -> Color
addColor (Color r g b) (Color r' g' b') = Color (r + r') (g + g') (b + b')

-- | Scales a color by scaling all its color channels
scaleColor :: Double -> Color -> Color
scaleColor t (Color r g b) = Color (t * r) (t * g) (t * b)

scaleColor' :: Color -> Color -> Color
scaleColor' (Color r g b) (Color r' g' b') = Color (r * r') (g * g') (b * b')
