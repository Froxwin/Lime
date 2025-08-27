{-# LANGUAGE PackageImports #-}

module Linear.Metric (module L, UnitVec (UnitVec), mkUnitVec) where

import Linear.Epsilon (Epsilon)
import "linear" Linear.Metric as L
  ( Metric (..)
  , normalize
  , project
  )

-- | Wrapper for normalized vectors,
--   because life is too short for untracable bugs
newtype UnitVec v = UnitVec v deriving (Show, Eq)

mkUnitVec :: (Eq a, Metric v, Floating a, Epsilon a) => v a -> UnitVec (v a)
mkUnitVec v =
  if norm v == 0
    then error "Tried to normalize the zero vector"
    else UnitVec $ normalize v
