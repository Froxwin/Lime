{-# LANGUAGE ViewPatterns #-}

module Linear.Optics where

import Linear

-- | Reflect a vector wrt to the surface normal. The normal should be normalized
reflect :: (Floating a, Metric f, Epsilon a) => f a -> f a -> f a
reflect v (normalize -> n) = v ^-^ ((2 * (v `dot` n)) *^ n)

-- refract :: V3 Double -> V3 Double -> Double -> V3 Double
-- refract v (normalize -> n) mu = (norm v) *^ normalize $
--   (mu *^ (v - ((n `dot` v) *^ n)))
--     - sqrt (1 - ((mu ^ 2) * (1 - ((n `dot` v) ^ 2)))) *^ n

-- refract :: V3 Double -> V3 Double -> Double -> V3 Double
-- refract v (normalize -> n) mu = if t1 >= tcrit then error "qwha" else norm v *^ (((mu *^ l) ^+^ (((mu * c) - sqrt (1 - ((mu ^ 2) * (1 - (c ^ 2))))) *^ n)))
--   where
--     t1 = - acos (n `dot` v)
--     tcrit = asin (mu)
--     l = normalize v
--     c = -(n `dot` l)

refract :: V3 Double -> V3 Double -> Double -> V3 Double
refract v (normalize -> n) mu =
  if theta >= crit
    then error "Cannot Refract"
    else
      norm v *^ (((-sqrt (1 - ((sine / mu) ^ 2))) *^ n) ^+^ ((sine / mu) *^ perp))
 where
  crit = asin (1 / mu)
  perp = normalize ((n `cross` normalize v) `cross` n)
  theta = acos (negated (normalize v) `dot` n)
  sine = sin theta
