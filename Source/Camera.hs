{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards  #-}

module Camera where

import           Codec.Picture    (DynamicImage, Image, PixelRGB8 (PixelRGB8),
                                   PixelRGBF (..), convertRGB8, pixelMap)
import           Codec.Wavefront  (WavefrontOBJ)
import           Data.Aeson.Types (FromJSON (parseJSON), Parser, Value)
import           Data.List        (sortBy)
import           Data.Map         (Map)
import qualified Data.Map         as M
import           Data.Maybe       (fromJust, isJust, mapMaybe)
import           GHC.Generics     (Generic)

import           Color            (Color (..), addColor, correctGamma,
                                   scaleColor, scaleColor')
import           Primitives       (Primitive, TWorldObject (primitive),
                                   WorldObject)
import           Ray              (Ray (Ray))
import           Textures         (WorldTexture, texture)
import           Utils            (worldParse)
import           Vector           (Vec3 (Vec3), cross, dot, normalize, vadd,
                                   vdiv, vmul, vneg, vsub)

rayColor
  :: Ray
  -> Map String (Image PixelRGBF)
  -> [Primitive]
  -> Vec3
  -> Int
  -> WorldTexture
  -> Color Double
rayColor ray@(Ray origin direction) texs objs sample depth background
  | depth == 0 = Color 0 0 0
  | null hits = texture texs background (getUV unitDir) unitDir
  | isJust reflected = color `scaleColor'` reflectedColor
  | otherwise = color
 where
  reflectedColor =
    rayColor (fromJust reflected) texs objs sample (depth - 1) background
  unitDir = normalize direction
  getUV (Vec3 px py pz) = (phi / (2 * pi), theta / pi)
   where
    theta = acos (-py)
    phi = atan2 (-pz) px + pi
  v = if sample `dot` normal > 0 then sample else vneg sample
  (color, reflected) = material texs texCoords ray normal point v
  (normal, point, _, material, texCoords) = head hits
  hits =
    sortBy
      ( \(_, p1, _, _, _) (_, p2, _, _, _) ->
          compare (p1 `vsub` origin) (p2 `vsub` origin)
      )
      $ mapMaybe (\prim -> prim ray 0.001 (1 / 0)) objs

-- |
-- The 'render' function generates a list of rays directed from the lens aperture
-- to the viewport.
--
-- The generation is done __left-to-right__ and __top-to-bottom__.
render
  :: Scene
  -> Map String WavefrontOBJ
  -> Map String DynamicImage
  -> [Color Double]
render (Scene width height samples bounces (Camera {..}) _ _ world) objs texs =
  [ correctGamma $
    foldl
      addColor
      (Color 0 0 0)
      [ (1 / (fromIntegral (length sampleCube) ^ 3))
        `scaleColor` rayColor
          (Ray diskSample (pixelSample `vsub` diskSample))
          textureImgs
          (concatMap (primitive objs) world)
          (Vec3 sx sy sz)
          bounces
          backgroundTexture
      | sx <- sampleCube
      , sy <- sampleCube
      , let pixelSample =
              bottomLeft
                `vadd` ((ui + (sx / fromIntegral width)) `vmul` horizontal)
                `vadd` ((vi + (sy / fromIntegral height)) `vmul` vertical)
      , let diskSample =
              position `vadd` (sx `vmul` diskU) `vadd` (sy `vmul` diskV)
      , sz <- sampleCube
      ]
  | vi <- reverse $ scanline $ fromIntegral height
  , ui <- scanline $ fromIntegral width
  ]
 where
  scanline q = map (/ q) [0 .. q - 1]
  sampleCube = [-1, (-1 + (1 / ((((samples ** (1 / 3)) - 3) / 2) + 1))) .. 1]
  textureImgs =
    M.map
      ( pixelMap
          ( \(PixelRGB8 r g b) ->
              PixelRGBF
                (fromIntegral (toInteger r) / 255)
                (fromIntegral (toInteger g) / 255)
                (fromIntegral (toInteger b) / 255)
          )
          . convertRGB8
      )
      texs

  -- camera
  w = normalize (position `vsub` lookingAt)
  u = normalize $ cross upwardVector w
  v = cross w u

  -- viewport
  viewportHeight = 2 * tan (fieldOfView / 2) * focalLength
  viewportWidth = viewportHeight * (fromIntegral width / fromIntegral height)
  horizontal = viewportWidth `vmul` u
  vertical = viewportHeight `vmul` v
  bottomLeft =
    position
      `vsub` (2 `vdiv` horizontal)
      `vsub` (2 `vdiv` vertical)
      `vsub` (focalLength `vmul` w)

  -- aperture
  diskRadius = focalLength * tan (defocusAngle / 2)
  diskU = diskRadius `vmul` u
  diskV = diskRadius `vmul` v

-- | Represents camera configuration
data Camera = Camera
  { position :: Vec3
  , lookingAt :: Vec3
  , focalLength :: Double
  , fieldOfView :: Double
  , upwardVector :: Vec3
  , defocusAngle :: Double
  , backgroundTexture :: WorldTexture
  }
  deriving (Show, Generic, Eq)

instance FromJSON Camera where
  parseJSON :: Value -> Parser Camera
  parseJSON = worldParse

-- | Represents a world scene
data Scene = Scene
  { width :: Int
  , height :: Int
  , samplesPerPixel :: Double
  , maximumBounces :: Int
  , camera :: Camera
  , textures :: Maybe [(String, FilePath)]
  , objects :: Maybe [(String, FilePath)]
  , world :: [WorldObject]
  }
  deriving (Show, Generic, Eq)

instance FromJSON Scene where
  parseJSON :: Value -> Parser Scene
  parseJSON = worldParse
