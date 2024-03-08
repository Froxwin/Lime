{-# LANGUAGE DeriveGeneric #-}

module Color where

import           Data.Yaml                      ( FromJSON )
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

instance FromJSON Color
