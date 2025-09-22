{-# LANGUAGE FlexibleInstances #-}

module Data.Color where

import Control.DeepSeq (NFData)
import Data.Aeson.Types (FromJSON (parseJSON), Parser, Value)
import GHC.Generics (Generic)

data Color a = Color !a !a !a
  deriving (Show, Eq, Ord, Generic)

instance Functor Color where
  {-# INLINE fmap #-}
  fmap :: (a -> b) -> Color a -> Color b
  fmap f (Color r g b) = Color (f r) (f g) (f b)

instance Applicative Color where
  {-# INLINE pure #-}
  pure :: a -> Color a
  pure x = Color x x x
  {-# INLINE (<*>) #-}
  (<*>) :: Color (a -> b) -> Color a -> Color b
  (<*>) (Color fr fg fb) (Color r g b) = Color (fr r) (fg g) (fb b)

{-
instance RealFrac a => Show (Color a) where
  show :: Color a -> String
  show (Color r g b) = show $ map (round . (* 255)) [r, g, b]
-}

-- | Implicitly converts RGB pixel values [0 .. 255] to high dynamic range pixel
-- values [0.0 .. 1.0]
instance (FromJSON a, Fractional a) => FromJSON (Color a) where
  parseJSON :: Value -> Parser (Color a)
  parseJSON v = do
    [x, y, z] <- parseJSON v
    return $ (/ 255) <$> Color x y z

instance NFData a => NFData (Color a)

scale :: Num a => a -> Color a -> Color a
scale t color = (* t) <$> color
{-# INLINE scale #-}

newtype ColorSum a = ColorSum {getColorSum :: a}
  deriving (Show, Eq, Ord)

instance (Num a, Applicative t) => Semigroup (ColorSum (t a)) where
  {-# INLINE (<>) #-}
  (<>) :: ColorSum (t a) -> ColorSum (t a) -> ColorSum (t a)
  (<>) (ColorSum color) (ColorSum color') = ColorSum $ (+) <$> color <*> color'

instance (Num a, Applicative t) => Monoid (ColorSum (t a)) where
  {-# INLINE mempty #-}
  mempty :: ColorSum (t a)
  mempty = ColorSum $ pure 0

newtype ColorProduct a = ColorProduct {getColorProduct :: a}
  deriving (Show, Eq, Ord)

instance (Num a, Applicative t) => Semigroup (ColorProduct (t a)) where
  {-# INLINE (<>) #-}
  (<>) :: ColorProduct (t a) -> ColorProduct (t a) -> ColorProduct (t a)
  (<>) (ColorProduct color) (ColorProduct color') =
    ColorProduct $ (*) <$> color <*> color'

instance (Num a, Applicative t) => Monoid (ColorProduct (t a)) where
  {-# INLINE mempty #-}
  mempty :: ColorProduct (t a)
  mempty = ColorProduct $ pure 1
