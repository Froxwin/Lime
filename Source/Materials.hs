module Materials where

import           Color
import           Ray
import           Vector

type Material = Ray -> Vector -> Vector -> Vector -> (Color, Ray)

lambertian :: Color -> Material
lambertian c r n p v = (c, Ray p (v `vadd` n))

metal :: Color -> Material
metal c r n p v = (c, Ray p (reflect (normalize (rayDirection r)) n))
