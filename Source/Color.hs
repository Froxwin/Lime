module Color where

data Color = Color
  { red   :: Double
  , green :: Double
  , blue  :: Double
  }
  deriving Eq

instance Show Color where
  show :: Color -> String
  show (Color r g b) = concat $ (flip $ zipWith (++)) [" ", " ", ""] $ map
    (show . round)
    [255.9 * r / 9, 255.9 * g / 9, 255.9 * b / 9]

addColor :: Color -> Color -> Color
(Color r g b) `addColor` (Color r' g' b') = Color (r + r') (g + g') (b + b')
