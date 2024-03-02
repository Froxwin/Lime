{-# LANGUAGE DeriveGeneric #-}

module Camera where

import           Data.List                      ( sortBy )
import           Data.Maybe                     ( mapMaybe )
import           Data.Yaml                      ( FromJSON )
import           GHC.Generics                   ( Generic )
import           Ray                            ( Ray(Ray, rayDirection) )
import           Things.Thing                   ( parseWorldObject )
import           Things.Types                   ( Thing
                                                , WorldObject
                                                )
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
render :: Scene -> [Color]
render (Scene imgWidth imgHeight nsamples (Camera origin lookAt fl f up da) wrld)
  = [ foldl
        addColor
        (Color 0 0 0)
        [ (1 / (fromIntegral (length sampleSquare) ^ 2)) `scaleColor` rayColor
            (Ray diskSample (pixelSample `vsub` diskSample))
            (map parseWorldObject wrld)
        | sx <- sampleSquare
        , sy <- sampleSquare
        , let pixelSample =
                bottomLeft
                  `vadd` ((ui + (sx / imgWidth)) `vmul` horizontal)
                  `vadd` ((vi + (sy / imgHeight)) `vmul` vertical)
        , let diskSample =
                origin `vadd` (sx `vmul` diskU) `vadd` (sy `vmul` diskV)
        ]
    | vi <- reverse $ scanline imgHeight
    , ui <- scanline imgWidth
    ]
 where
  scanline q = map (/ q) [0 .. q - 1]
  sampleSquare =
    [-1, (-1 + (1 / ((((nsamples ** (1 / 2)) - 3) / 2) + 1))) .. 1]

  -- camera
  w              = normalize (origin `vsub` lookAt)
  u              = normalize $ cross up w
  v              = cross w u

  -- viewport
  viewportHeight = 2 * tan (f / 2) * fl
  viewportWidth  = viewportHeight * (imgWidth / imgHeight)
  horizontal     = viewportWidth `vmul` u
  vertical       = viewportHeight `vmul` v
  bottomLeft =
    origin
      `vsub` (2 `vdiv` horizontal)
      `vsub` (2 `vdiv` vertical)
      `vsub` (fl `vmul` w)

  -- aperture
  diskRadius = fl * tan (da / 2)
  diskU      = diskRadius `vmul` u
  diskV      = diskRadius `vmul` v

-- | Represents camera configuration
data Camera = Camera
  { position     :: Vector -- ^ Point from where the camera is looking
  , lookingAt    :: Vector -- ^ Point in which direction the camera is looking
  , focalLength  :: Double -- ^ Focal length of camera
  , fov          :: Double -- ^ Horizontal field of view angle
  , upVector     :: Vector -- ^ Upward direction of camera
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
  , world   :: [WorldObject]
  }
  deriving (Show, Generic)

instance FromJSON Scene

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

-- | Adds two colors by adding their respective rgb components
addColor :: Color -> Color -> Color
addColor (Color r g b) (Color r' g' b') = Color (r + r') (g + g') (b + b')

-- | Scales a color by scaling all its color channels
scaleColor :: Double -> Color -> Color
scaleColor t (Color r g b) = Color (t * r) (t * g) (t * b)
