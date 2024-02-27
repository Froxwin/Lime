{-# LANGUAGE DeriveGeneric #-}

module Camera where

import           Data.List                      ( sortBy )
import           Data.Maybe                     ( mapMaybe )
import           Data.Yaml                      ( FromJSON )
import           GHC.Generics                   ( Generic )
import           Ray                            ( Ray(Ray, rayDirection) )
import           Vector                         ( Vector(Vector, vy)
                                                , cross
                                                , normalize
                                                , vadd
                                                , vdiv
                                                , vmul
                                                , vsub
                                                )

-- | The 'rayColor' function computes the color of a ray.
rayColor :: Ray -> [Thing] -> Color
rayColor r ls =
  let (Vector tx ty tz)
        | not (null hits) = 0.5 `vmul` (head hits `vadd` Vector 1 1 1)
        | otherwise = ((1 - d) `vmul` bottomColor) `vadd` (d `vmul` topColor)
  in  Color tx ty tz
 where
  hitting ray tMin tMax f = f ray tMin tMax
  d    = 0.5 * (vy (normalize $ rayDirection r) + 1.0)
  hits = map fst $ sortBy (\(_, p1) (_, p2) -> compare p1 p2) $ mapMaybe
    (hitting r 0 (1 / 0))
    ls
  bottomColor = Vector 1.0 1.0 1.0
  topColor    = Vector 0.5 0.7 1.0

{-|
  The 'render' function generates a list of rays directed from the camera to the
  viewport.

  The generation is done __left-to-right__ and __top-to-bottom__.

  The function implements anti-aliasing via grid supersampling with sample size
  'samplesPerPixel'.
-}
render :: Scene -> [Thing] -> [Color]
render s ls =
  [ foldl
      addColor
      (Color 0 0 0)
      [ (1 / samplesPerPixel) `scaleColor` rayColor (Ray camOrigin direction) ls
      | sx <- sampleSquare
      , sy <- sampleSquare
      , let pixelSample =
              bottomLeft
                `vadd` ((ui + (sx / imgWidth)) `vmul` horizontal)
                `vadd` ((vi + (sy / imgHeight)) `vmul` vertical)
      , let rayOrigin = camOrigin
      , let direction = pixelSample `vsub` rayOrigin
      ]
  | vi <- reverse $ scanline imgHeight
  , ui <- scanline imgWidth
  ]
 where
  scanline q = map (/ q) [0 .. q - 1]
  sampleSquare =
    [-1, (-1 + (1 / ((((samplesPerPixel ** (1 / 2)) - 3) / 2) + 1))) .. 1]
  imgWidth        = width s
  imgHeight       = height s
  aspectRatio     = width s / height s
  samplesPerPixel = samples s

  -- camera
  Camera camOrigin looking fl f up da = camera s
  w               = normalize (camOrigin `vsub` looking)
  u               = normalize $ cross up w
  v               = cross w u

  -- viewport
  h               = tan (f / 2)
  viewportHeight  = 2 * h * fl
  viewportWidth   = viewportHeight * aspectRatio
  horizontal      = viewportWidth `vmul` u
  vertical        = viewportHeight `vmul` v
  bottomLeft =
    camOrigin
      `vsub` (2 `vdiv` horizontal)
      `vsub` (2 `vdiv` vertical)
      `vsub` (fl `vmul` w)

  -- TODO add defocus blurring
  -- FIXME the supersampler acts weird sometimes

-- | Represents camera configuration
data Camera = Camera
  { cameraOrigin :: Vector -- ^ Point from where the camera is looking
  , lookingAt    :: Vector -- ^ Point in which direction the camera is looking
  , focalLength  :: Double -- ^ Focal length of camera
  , fov          :: Double -- ^ Horizontal field of view angle
  , cameraUpVec  :: Vector -- ^ Upward direction of camera
  , defocusAngle :: Double -- ^ Angle subtended by lens aperture
  }
  deriving (Show, Generic)

instance FromJSON Camera

-- | Represents a world scene
data Scene = Scene
  { width   :: Double -- ^ The width of rendered image
  , height  :: Double -- ^ The height of rendered image
  , samples :: Double -- ^ Number of samples per pixel
  , camera  :: Camera -- ^ Configuration of camera
  }
  deriving (Show, Generic)

instance FromJSON Scene

-- | Represents a rgb color type
data Color = Color
  { red   :: Double -- ^ The red component of a color
  , green :: Double -- ^ The green component of a color
  , blue  :: Double -- ^ The blue component of a color
  }
  deriving Eq

instance Show Color where
  show :: Color -> String
  show (Color r g b) = concat $ (flip $ zipWith (++)) [" ", " ", ""] $ map
    (show . round . (* 255))
    [r, g, b]

-- | Adds two colors by adding their respective rgb components
addColor :: Color -> Color -> Color
addColor (Color r g b) (Color r' g' b') = Color (r + r') (g + g') (b + b')

-- | Scales a color by scaling all its color channels
scaleColor :: Double -> Color -> Color
scaleColor t (Color r g b) = Color (t * r) (t * g) (t * b)

type Thing = Ray -> Double -> Double -> Maybe (Vector, Vector)
