module Color where

import Control.DeepSeq  (NFData)
import Data.Aeson.Types (FromJSON (parseJSON), Parser, Value)
import GHC.Generics     (Generic)

-- | Represents a rgb color type with fields representing respective color
--  channels
data Color a = Color a a a
  deriving (Eq, Generic)

instance Functor Color where
  fmap :: (a -> b) -> Color a -> Color b
  fmap f (Color r g b) = Color (f r) (f g) (f b)

instance Applicative Color where
  pure :: a -> Color a
  pure x = Color x x x
  (<*>) :: Color (a -> b) -> Color a -> Color b
  (<*>) (Color fr fg fb) (Color r g b) = Color (fr r) (fg g) (fb b)

instance RealFrac a => Show (Color a) where
  show :: Color a -> String
  show (Color r g b) = show $ map (round . (* 255)) [r, g, b]

instance (FromJSON a, Fractional a) => FromJSON (Color a) where
  parseJSON :: Value -> Parser (Color a)
  parseJSON v = do
    [x, y, z] <- parseJSON v
    return $ (/ 255) <$> Color x y z

instance NFData a => NFData (Color a)

colorDouble2Word :: (RealFrac a, Integral b) => Color a -> Color b
colorDouble2Word color = round . (* 255) <$> color

colorWord2Double :: (Fractional b, Integral a) => Color a -> Color b
colorWord2Double color = (/ 255) . fromIntegral <$> color

-- | Convert color from linear space to gamma space with γ = 1/2
correctGamma :: Floating a => Color a -> Color a
correctGamma color = sqrt <$> color

-- | Adds two colors by adding their respective rgb components
addColor :: Num a => Color a -> Color a -> Color a
addColor color color' = (+) <$> color <*> color'

-- | Scales a color by scaling all its color channels
scaleColor :: Num a => a -> Color a -> Color a
scaleColor t color = (* t) <$> color

-- | Scales a color by multiplying its color channels with the given color
scaleColor' :: Num a => Color a -> Color a -> Color a
scaleColor' color color' = (*) <$> color <*> color'
