module Materials where

import           Color  (Color (Color))
import           Ray    (Ray (Ray, rayDirection))
import           Vector (Vector, dot, normalize, reflect, refract, vadd, vmul,
                         vneg)
import Textures (Texture, TextureCoords, solidColor)

type Material = TextureCoords -> Ray -> Vector -> Vector -> Vector -> (Color, Maybe Ray)

lambertian :: Texture -> Material
lambertian tex (ut, vt) _ n p v = (tex (ut,vt) p, Just $ Ray p (v `vadd` n))

metal :: Texture -> Double -> Material
metal tex f (ut, vt) r n p v =
  (tex (ut,vt) p, Just $ Ray p (reflect (normalize (rayDirection r)) n `vadd` (f `vmul` v)))

dielectric :: Double -> Material
dielectric ior _ r n p _ = (Color 1 1 1, Just $ Ray p direction)
 where
  ratio     = if rayDirection r `dot` n < 0 then 1 / ior else ior
  direction = if rayDirection r `dot` n > 0
    then refract (normalize (rayDirection r)) (vneg n) ratio
    else refract (normalize (rayDirection r)) n ratio

diffuseLight :: Color -> Material
diffuseLight color (u, v) _ _ p _ = (solidColor color (u, v) p, Nothing)
