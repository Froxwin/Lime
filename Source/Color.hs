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
    [255.9 * r, 255.9 * g, 255.9 * b]
