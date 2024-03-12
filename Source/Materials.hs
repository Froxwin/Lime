module Materials where

import           Color  (Color (Color))
import           Ray    (Ray (Ray, rayDirection))
import           Vector (Vector, dot, normalize, reflect, refract, vadd, vmul,
                         vneg)

type Material = Ray -> Vector -> Vector -> Vector -> (Color, Ray)

lambertian :: Color -> Material
lambertian c _ n p v = (c, Ray p (v `vadd` n))

metal :: Color -> Double -> Material
metal c f r n p v =
  (c, Ray p (reflect (normalize (rayDirection r)) n `vadd` (f `vmul` v)))

dielectric :: Double -> Material
dielectric ior r n p _ = (Color 1 1 1, Ray p direction)
 where
  ratio     = if rayDirection r `dot` n < 0 then 1 / ior else ior
  direction = if rayDirection r `dot` n > 0
    then refract (normalize (rayDirection r)) (vneg n) ratio
    else refract (normalize (rayDirection r)) n ratio
