{-# LANGUAGE DeriveGeneric #-}

module Camera where

import           Data.List                      ( sortBy )
import           Data.Maybe                     ( mapMaybe )
import           Data.Yaml                      ( FromJSON )
import           GHC.Generics                   ( Generic )
import           Ray                            ( Ray(Ray, direction) )
import           Vector                         ( (<+>)
                                                , (<->)
                                                , (<.>)
                                                , (</>)
                                                , Vector(Vector, vy)
                                                , cross
                                                , unitVector
                                                )

data Camera = Camera
  { origin      :: Vector
  , looking     :: Vector
  , focalLength :: Double
  , fov         :: Double
  , upVec       :: Vector
  }
  deriving (Show, Generic)

instance FromJSON Camera

data Scene = Scene
  { width   :: Double
  , height  :: Double
  , samples :: Double
  , camera  :: Camera
  }
  deriving (Show, Generic)

instance FromJSON Scene

bottomColor, topColor :: Vector
bottomColor = Vector 1.0 1.0 1.0
topColor = Vector 0.5 0.7 1.0

type Thing = Ray -> Double -> Double -> Maybe (Vector, Vector)

-- | The 'rayColor' function computes the color of a ray.
rayColor :: Ray -> [Thing] -> Color
rayColor r ls =
  let (Vector tx ty tz)
        | not (null hits) = 0.5 <.> (head hits <+> Vector 1 1 1)
        | otherwise       = ((1 - d) <.> bottomColor) <+> (d <.> topColor)
  in  Color tx ty tz
 where
  hitting ray tMin tMax f = f ray tMin tMax
  d    = 0.5 * (vy (unitVector $ direction r) + 1.0)
  hits = map fst $ sortBy (\(_, p1) (_, p2) -> compare p1 p2) $ mapMaybe
    (hitting r 0 (1 / 0))
    ls

{-|
  The 'render' function generates a list of rays directed from the camera to the
  viewport.

  The generation is done __left-to-right__ and __top-to-bottom__.

  The function implements anti-aliasing via grid supersampling with sample size
  'samplesPerPixel'.
-}
render :: Scene -> [Thing] -> [Color]
render s ls =
  [ (\(Color r g b) ->
      Color (r / samplesPerPixel) (g / samplesPerPixel) (b / samplesPerPixel)
    )
      $ foldl
          addColor
          (Color 0 0 0)
          [ rayColor (Ray cameraOrigin rayDirection) ls
          | sx <- sampleSquare
          , sy <- sampleSquare
          , let rayDirection =
                  bottomLeft
                    <+> ((ui + (sx / imgWidth)) <.> horizontal)
                    <+> ((vi + (sy / imgWidth)) <.> vertical)
                    <-> cameraOrigin
          ]
  | vi <- reverse $ scanline imgHeight
  , ui <- scanline imgWidth
  ]
 where
  scanline q = map (/ q) [0 .. q - 1]
  sampleSquare =
    [-(sqrt samplesPerPixel - 1) / 2 .. (sqrt samplesPerPixel - 1) / 2]
  imgWidth        = width s
  imgHeight       = height s
  aspectRatio     = width s / height s
  samplesPerPixel = samples s

  -- camera
  Camera cameraOrigin lookingAt fl f up = camera s
  w               = unitVector (cameraOrigin <-> lookingAt)
  u               = unitVector $ cross up w
  v               = cross w u

  -- viewport
  h               = tan (f / 2)
  viewportWidth   = viewportHeight * aspectRatio
  viewportHeight  = 2 * h * fl
  horizontal      = viewportWidth <.> u
  vertical        = viewportHeight <.> v
  bottomLeft =
    cameraOrigin <-> (2 </> horizontal) <-> (2 </> vertical) <-> (fl <.> w)

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
(Color r g b) `addColor` (Color r' g' b') = Color (r + r') (g + g') (b + b')
