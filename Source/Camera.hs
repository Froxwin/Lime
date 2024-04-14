module Camera where

import Codec.Picture   (DynamicImage)
import Codec.Wavefront (WavefrontOBJ)
import Data.List       (sortBy)
import Data.Map        (Map)
import Data.Maybe      (fromJust, isJust, mapMaybe)
import Data.Yaml       (FromJSON (parseJSON), Parser, Value)
import GHC.Generics    (Generic)

import Color           (Color (..), addColor, correctGamma, scaleColor,
                        scaleColor')
import Primitives      (Primitive, TWorldObject (primitive), WorldObject)
import Ray             (Ray (Ray, rayOrigin))
import Utils           (worldParse)
import Vector          (Vec3 (Vec3), cross, dot, normalize, vadd, vdiv, vmul,
                        vneg, vsub)

rayColor :: Ray -> [Primitive] -> Vec3 -> Int -> Color Double -> Color Double
rayColor r ls sampleRay depth background
  | depth == 0 = Color 0 0 0
  | not (null hs) = if isJust w
    then
      q `scaleColor'` rayColor (fromJust w) ls sampleRay (depth - 1) background
    else q
  | otherwise = background
 where
  v = if sampleRay `dot` n > 0 then sampleRay else vneg sampleRay
  hitting ray tMin tMax f = f ray tMin tMax
  (q, w)           = m tc r n p v
  (n, p, _, m, tc) = head hs
  hs =
    sortBy
        (\(_, p1, _, _, _) (_, p2, _, _, _) ->
          compare (p1 `vsub` rayOrigin r) (p2 `vsub` rayOrigin r)
        )
      $ mapMaybe (hitting r 0.001 (1 / 0)) ls

-- |
--  The 'render' function generates a list of rays directed from the camera to the
--  viewport.
--
--  The generation is done __left-to-right__ and __top-to-bottom__.
--
--  The function employs grid supersampling with sample size 'samplesPerPixel'.
render
  :: Scene
  -> Map String WavefrontOBJ
  -> Map String DynamicImage
  -> [Color Double]
render (Scene imgWidth imgHeight nsamples m (Camera origin lookAt fl f up da bg) _ _ wrld) objss _wq
  = [ correctGamma $ foldl
        addColor
        (Color 0 0 0)
        [ (1 / (fromIntegral (length sampleSquare) ^ 3))
            `scaleColor` rayColor
                           (Ray diskSample (pixelSample `vsub` diskSample))
                           (concatMap (primitive objss) wrld)
                           (Vec3 sx sy sz)
                           m
                           bg
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
  { camPosition        :: Vec3
  , camLookingAt       :: Vec3
  , camFocalLength     :: Double
  , camFieldOfView     :: Double
  , camUpwardVector    :: Vec3
  , camDefocusAngle    :: Double
  , camBackgroundColor :: Color Double
  }
  deriving (Show, Generic, Eq)

instance FromJSON Camera where
  parseJSON :: Value -> Parser Camera
  parseJSON = worldParse 3

-- | Represents a world scene
data Scene = Scene
  { width           :: Int
  , height          :: Int
  , samplesPerPixel :: Double
  , maximumBounces  :: Int
  , camera          :: Camera
  , textures        :: Maybe [(String, FilePath)]
  , objects         :: Maybe [(String, FilePath)]
  , world           :: [WorldObject]
  }
  deriving (Show, Generic, Eq)

instance FromJSON Scene where
  parseJSON :: Value -> Parser Scene
  parseJSON = worldParse 0
