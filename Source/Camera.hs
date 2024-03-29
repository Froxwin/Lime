{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Camera where

import           Codec.Picture                  ( DynamicImage )
import           Color                          ( Color(Color)
                                                , addColor
                                                , correctGamma
                                                , scaleColor
                                                , scaleColor'
                                                )
import           Data.List                      ( sortBy )
import           Data.Maybe                     ( fromJust
                                                , isJust
                                                , mapMaybe
                                                )
import           Data.Yaml                      ( (.:)
                                                , FromJSON(parseJSON)
                                                , Parser
                                                , Value(Object)
                                                )
import           GHC.Generics                   ( Generic )
import           Primitives                     ( Primitive
                                                , TWorldObject(tprimitive)
                                                , WorldObject
                                                )
import           Ray                            ( Ray(..) )
import           Vector                         ( Vector(Vector, vy)
                                                , cross
                                                , dot
                                                , normalize
                                                , vadd
                                                , vdiv
                                                , vmul
                                                , vneg
                                                , vsub
                                                )

-- | The 'rayColor' function computes the color of a ray.
rayColor :: Ray -> [Primitive] -> Vector -> Int -> Color
rayColor r ls sampleRay depth
  | depth == 0
  = Color 0 0 0
  | null hs
  = Color 0 0 0
  | not (null hs)
  = if isJust w
    then q `scaleColor'` rayColor (fromJust w) ls sampleRay (depth - 1)
    else q
  | otherwise
  = ((1 - d) `scaleColor` Color 1.0 1.0 1.0)
    `addColor` (d `scaleColor` Color 0.5 0.7 1.0)
 where
  v = if sampleRay `dot` n > 0 then sampleRay else vneg sampleRay
  hitting ray tMin tMax f = f ray tMin tMax
  (q, w)        = m tc r n p v
  (n, p, m, tc) = head hs
  hs =
    sortBy
        (\(_, p1, _, _) (_, p2, _, _) ->
          compare (p1 `vsub` rayOrigin r) (p2 `vsub` rayOrigin r)
        )
      $ mapMaybe (hitting r 0.001 (1 / 0)) ls
  d = 0.5 * (vy (normalize $ rayDirection r) + 1.0)

{-|
  The 'render' function generates a list of rays directed from the camera to the
  viewport.

  The generation is done __left-to-right__ and __top-to-bottom__.

  The function implements anti-aliasing via grid supersampling with sample size
  'samplesPerPixel'.
-}
render :: Scene -> [(String, DynamicImage)] -> [Color]
render (Scene imgWidth imgHeight nsamples m (Camera origin lookAt fl f up da) _ wrld) _wq
  = [ correctGamma $ foldl
        addColor
        (Color 0 0 0)
        [ (1 / (fromIntegral (length sampleSquare) ^ 3))
            `scaleColor` rayColor
                           (Ray diskSample (pixelSample `vsub` diskSample))
                           (map tprimitive wrld)
                           (Vector sx sy sz)
                           m
        | sx <- sampleSquare
        , sy <- sampleSquare
        , let pixelSample =
                bottomLeft
                  `vadd` ((ui + (sx / fromIntegral imgWidth)) `vmul` horizontal)
                  `vadd` ((vi + (sy / fromIntegral imgHeight)) `vmul` vertical)
        , let diskSample =
                origin `vadd` (sx `vmul` diskU) `vadd` (sy `vmul` diskV)
        , sz <- sampleSquare
        ]
    | vi <- reverse $ scanline $ fromIntegral imgHeight
    , ui <- scanline $ fromIntegral imgWidth
    ]
 where
  scanline q = map (/ q) [0 .. q - 1]
  sampleSquare =
    [-1, (-1 + (1 / ((((nsamples ** (1 / 3)) - 3) / 2) + 1))) .. 1]

  -- camera
  w              = normalize (origin `vsub` lookAt)
  u              = normalize $ cross up w
  v              = cross w u

  -- viewport
  viewportHeight = 2 * tan (f / 2) * fl
  viewportWidth =
    viewportHeight * (fromIntegral imgWidth / fromIntegral imgHeight)
  horizontal = viewportWidth `vmul` u
  vertical   = viewportHeight `vmul` v
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

instance FromJSON Camera where
  parseJSON :: Value -> Parser Camera
  parseJSON (Object v) =
    Camera
      <$> v
      .:  "position"
      <*> v
      .:  "looking-at"
      <*> v
      .:  "focal-length"
      <*> v
      .:  "field-of-view"
      <*> v
      .:  "upward-vector"
      <*> v
      .:  "defocus-angle"
  parseJSON _ = error "Can't parse Camera from YAML"

-- | Represents a world scene
data Scene = Scene
  { width     :: Int -- ^ The width of rendered image
  , height    :: Int -- ^ The height of rendered image
  , samples   :: Double -- ^ Number of samples per pixel
  , maxBounce :: Int
  , camera    :: Camera -- ^ Configuration of camera
  , textures  :: [(String, FilePath)]
  , world     :: [WorldObject]
  }
  deriving (Show, Generic)

instance FromJSON Scene where
  parseJSON :: Value -> Parser Scene
  parseJSON (Object v) =
    Scene
      <$> v
      .:  "width"
      <*> v
      .:  "height"
      <*> v
      .:  "samples-per-pixel"
      <*> v
      .:  "maximum-bounces"
      <*> v
      .:  "camera"
      <*> v
      .:  "texture-files"
      <*> v
      .:  "world"
  parseJSON _ = error "Can not parse Scene from YAML"
