{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Lime.Camera where

import Codec.Picture
  ( DynamicImage
  , Image
  , PixelRGB8 (PixelRGB8)
  , PixelRGBF (..)
  , convertRGB8
  , generateImage
  , pixelMap
  )
import Codec.Picture.Types (gammaCorrection)

-- import           Codec.Wavefront      (WavefrontOBJ)
import Data.Aeson.Types (FromJSON (parseJSON), Parser, Value)

-- import qualified Data.Map             as M
-- import           Data.Maybe           (mapMaybe)

import Data.Coerce
import Data.Color
import Data.List (sortBy)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Massiv.Array qualified as A
import Data.Massiv.Array.IO qualified as A
import Data.Maybe
import Data.Ray (Ray (Ray))
import Data.Vector (Vector)
import Data.Vector qualified as V
import Debug.Trace
import GHC.Generics (Generic)
import Graphics.ColorModel qualified as GC
import Lime.Internal.Hit
import Lime.Internal.Utils (worldParse)
import Lime.Materials (WorldMaterial (Emissive, Lambertian))
import Lime.Primitives
  ( IsHittable (primitive)
  , Primitive
  , Shape (material)
  )
import Lime.Textures (WorldTexture (SolidColor), texture)
import Linear (V2 (V2))
import Linear.Metric
import Linear.V3 (V3 (..), cross)
import Linear.Vector
  ( Additive ((^+^), (^-^))
  , negated
  , (*^)
  , (^/)
  )
import System.Random

rayColor
  :: Ray
  -> Map String (Image PixelRGBF)
  -> [Primitive]
  -> V3 Double -- StdGen -- V3 Double
  -> Int
  -> WorldTexture
  -> Color Double
rayColor ray@(Ray origin ( dir)) texs objs g depth background
  | depth == 0 = pure 0
  | null hits =
      texture
        texs
        background
        ( HitData
            { coords = getUV dir
            , point = dir
            , normal = error "Something terrible has occurred."
            , param = error "Something unspeakably wrong just happened."
            }
        )
  | otherwise =
      maybe
        color
        -- (getColorProduct . mconcat . map coerce . (color :) . pure)
        (getColorProduct . (ColorProduct color <>) . ColorProduct)
        reflectedColor
 where
  -- (gen, gen') = split g
  reflectedColor =
    (\t -> rayColor t texs objs g (depth - 1) background) <$> reflected
  getUV (V3 px py pz) = (phi / (2 * pi), theta / pi)
   where
    theta = acos (-py)
    phi = atan2 (-pz) px + pi
  -- v = if sample `dot` normal > 0 then sample else negated sample
  -- (rv, g') = randomR (V3 (-1) (-1) (-1), V3 1 1 1) gen
  -- sample =
  -- normalize rv

  (color, reflected) = material texs g
  hit@(HitData {normal}, material) = head hits
  hits =
    sortBy (\(h1, _) (h2, _) -> compare h1.param h2.param) $
      mapMaybe (($ (1 / 0)) . ($ 0.001) . ($ ray)) objs

-- rayColor :: V3 Double -> Int -> Ray -> Reader SceneConfig (Color Double)
-- rayColor sample depth ray@(Ray _ (normalize -> dir)) = do
--   scene <- ask
--   let hits =
--         sortBy (\h1 h2 -> compare h1.param h2.param) $
--           mapMaybe (\prim -> prim ray 0.001 (1 / 0)) scene.world
--   let hit@(HitData {normal, material}) = head hits
--   let v = if sample `dot` normal > 0 then sample else negated sample
--   let (color, reflected) = material scene.textures hit ray v
--   let reflectedColor =
--         flip runReader scene . rayColor sample (depth - 1) <$> reflected
--   let getUV (V3 x y z) = ((atan2 (-z) x + pi) / (2 * pi), acos (-y) / pi)
--   return $
--     if depth == 0
--       then pure 0
--       else
--         if null hits
--           then
--             texture
--               scene.textures
--               scene.camera.backgroundTexture
--               (HitData {coords = getUV dir, point = dir})
--           else
--             maybe
--               color
--               (getColorProduct . (ColorProduct color <>) . ColorProduct)
--               reflectedColor

data Render = Render
  { width :: !Int
  , height :: !Int
  , stream :: !(Vector (Color Double))
  }

-- |
-- The 'render' function generates a list of rays directed from the lens aperture
-- to the viewport.
--
-- The generation is done __left-to-right__ and __top-to-bottom__.
render
  :: Scene
  -> StdGen
  -> Map String (Image PixelRGBF)
  -> [Color Double]
render (Scene width height samples bounces (Camera {..}) _ _ world) gen texs =
  [ fmap (** (1 / 2.2)) $
    getColorSum $
      mconcat
        [ ColorSum $
          (1 / (fromIntegral (length sampleCube) ^ 3))
            `scale` rayColor
              (Ray diskSample (normalize (pixelSample ^-^ diskSample)))
              textureImgs
              (concatMap (primitive) world)
              (V3 sx sy sz) -- gen
              bounces
              backgroundTexture
        | sx <- sampleCube
        , sy <- sampleCube
        , let pixelSample =
                bottomLeft
                  ^+^ ((ui + (sx / fromIntegral width)) *^ horizontal)
                  ^+^ ((vi + (sy / fromIntegral height)) *^ vertical)
        , let diskSample =
                position ^+^ (sx *^ diskU) ^+^ (sy *^ diskV)
        , sz <- sampleCube
        ]
  | vi <- reverse $ scanline $ fromIntegral height
  , ui <- scanline $ fromIntegral width
  ]
 where
  scanline q = map (/ q) [0 .. q - 1]
  sampleCube = [-1, (-1 + (1 / ((((samples ** (1 / 3)) - 3) / 2) + 1))) .. 1]
  textureImgs = texs

  -- camera
  w = normalize (position ^-^ lookingAt)
  u = normalize $ cross upwardVector w
  v = cross w u

  -- viewport
  viewportHeight = 2 * tan (fieldOfView / 2) * focalLength
  viewportWidth = viewportHeight * (fromIntegral width / fromIntegral height)
  horizontal = viewportWidth *^ u
  vertical = viewportHeight *^ v
  bottomLeft =
    position ^-^ (horizontal ^/ 2) ^-^ (vertical ^/ 2) ^-^ (focalLength *^ w)

  -- aperture
  diskRadius = focalLength * tan (defocusAngle / 2)
  diskU = diskRadius *^ u
  diskV = diskRadius *^ v

-- |
-- The 'render' function generates a list of rays directed from the lens aperture
-- to the viewport.
--
-- The generation is done __left-to-right__ and __top-to-bottom__.
--
-- The world uses a right handed coordinate system.
-- render
--   :: RandomGen p
--   => p
--   -> Scene
--   -> Map String (Image PixelRGBF)
--   -> A.Image A.S GC.RGB Double
-- render gen Scene {camera = Camera {..}, ..} ts =
--   A.makeArrayR
--     A.S
--     A.Par
--     (A.Sz (height A.:. width))
--     ( \(i A.:. j) ->
--         let ui = fromIntegral i; vi = fromIntegral j
--          in (\(Color r g b) -> GC.Pixel $ GC.ColorRGB r g b) $
--               getColorSum $
--                 mconcat
--                   [ ColorSum $
--                     (1 / (fromIntegral (length vsample)))
--                       -- (1 / (fromIntegral (length sampleCube) ^ 3))
--                       `scale` rayColor
--                         (Ray diskSample (pixelSample ^-^ diskSample))
--                         ts
--                         (concatMap (primitive) world)
--                         g'
--                         maximumBounces
--                         backgroundTexture
--                   | ((V3 sx sy sz), g) <- the -- sampleCube
--                   -- , sy <- vsample -- sampleCube
--                   , let ((V2 dx dy), g') = randomR ((V2 (-1) (-1)) :: V2 Double, (V2 1 1) :: V2 Double) (mkStdGen g)
--                   , let pixelSample =
--                           bottomLeft
--                             ^+^ ((ui + (sx / fromIntegral width)) *^ horizontal)
--                             ^+^ ((vi + (sy / fromIntegral height)) *^ vertical)
--                   , let diskSample = position ^+^ (dx *^ diskU) ^+^ (dy *^ diskV)
--                   -- , sz <- vsample -- sampleCube
--                   ]
--     )
--  where
--   -- trace
--   --   ("nsamples: " <> show (length vsample) <> "\n" <> "samples: " <> show vsample)
--   --   $
--   --   -- gammaCorrection 2 $
--   --   Render width height $ V.fromList $ A.toList
--   --   $ A.map
--   --     ( \(vi, ui) ->
--   --         getColorSum $
--   --           mconcat
--   --             [ ColorSum $
--   --               (1 / (fromIntegral (length vsample)))
--   --                 -- (1 / (fromIntegral (length sampleCube) ^ 3))
--   --                 `scale` rayColor
--   --                   (Ray diskSample (pixelSample ^-^ diskSample))
--   --                   ts
--   --                   (concatMap (primitive ms) world)
--   --                   g'
--   --                   maximumBounces
--   --                   backgroundTexture
--   --             | ((V3 sx sy sz), g) <- the -- sampleCube
--   --             -- , sy <- vsample -- sampleCube
--   --             , let ((V2 dx dy), g') = randomR ((V2 (-1) (-1)) :: V2 Double, (V2 1 1) :: V2 Double) (mkStdGen g)
--   --             , let pixelSample =
--   --                     bottomLeft
--   --                       ^+^ ((ui + (sx / fromIntegral width)) *^ horizontal)
--   --                       ^+^ ((vi + (sy / fromIntegral height)) *^ vertical)
--   --             , let diskSample = position ^+^ (dx *^ diskU) ^+^ (dy *^ diskV)
--   --             -- , sz <- vsample -- sampleCube
--   --             ]
--   --     )
--   -- (A.fromLists' A.Par' (V.toList ( V.map (,) (V.reverse $ scanline (fromIntegral height))
--   --     <*> scanline (fromIntegral width)
--   -- )) :: A.Array A.U A.Ix1 (Double, Double))

--   -- theGlazing =
--   --   SceneConfig
--   --     { camera = Camera {..}
--   --     , textures = ts
--   --     , models = ms
--   --     , world = concatMap (primitive ms) world
--   --     , ..
--   --     }
--   scanline q = V.map (/ q) [0 .. q - 1]

--   -- sampleCube =
--   -- [-1, (-1 + (1 / ((((samplesPerPixel ** (1 / 3)) - 3) / 2) + 1))) .. 1]

--   -- vsample = take (round (samplesPerPixel ** (1 / 3))) $ randomRs ((-1) :: Double, 1 :: Double) gen

--   the = zip vsample gens

--   gens = take (round (samplesPerPixel)) $ randoms gen :: [Int]

--   vsample =
--     take (round (samplesPerPixel)) $
--       randomRs ((V3 (-1) (-1) (-1)) :: V3 Double, (V3 1 1 1) :: V3 Double) gen

--   -- samples = [0 .. (round samplesPerPixel)] :: [Int]

--   -- camera
--   w = normalize (position ^-^ lookingAt)
--   u = normalize $ cross upwardVector w
--   v = cross w u

--   -- viewport
--   viewportHeight = 2 * tan (fieldOfView / 2) * focalLength
--   viewportWidth = viewportHeight * (fromIntegral width / fromIntegral height)
--   horizontal = viewportWidth *^ u
--   vertical = viewportHeight *^ v
--   bottomLeft =
--     position ^-^ (horizontal ^/ 2) ^-^ (vertical ^/ 2) ^-^ (focalLength *^ w)

--   -- aperture
--   diskRadius = focalLength * tan (defocusAngle / 2)
--   diskU = diskRadius *^ u
--   diskV = diskRadius *^ v

convertToPreview :: Scene -> Scene
convertToPreview scene =
  scene
    { height = 255
    , width = 400
    , samplesPerPixel = 50
    , maximumBounces = 2
    , camera = scene.camera {backgroundTexture = SolidColor (Color 1 1 1)}
    , world =
        map
          ( \x -> case x.material of
              Emissive _ -> x
              _ ->
                x {Lime.Primitives.material = Lambertian (SolidColor (pure 0.6))}
          )
          scene.world
    }

-- | Represents camera configuration
data Camera = Camera
  { position :: !(V3 Double)
  , lookingAt :: !(V3 Double)
  , focalLength :: !Double
  , fieldOfView :: !Double
  , upwardVector :: !(V3 Double)
  , defocusAngle :: !Double
  , backgroundTexture :: !WorldTexture
  }
  deriving (Show, Generic, Eq)

instance FromJSON Camera where
  parseJSON :: Value -> Parser Camera
  parseJSON = worldParse

-- | Represents a world scene
data Scene = Scene
  { width :: !Int
  , height :: !Int
  , samplesPerPixel :: !Double
  , maximumBounces :: !Int
  , camera :: !Camera
  , textures :: Maybe [(String, FilePath)]
  , models :: Maybe [(String, FilePath)]
  , world :: ![Shape]
  }
  deriving (Show, Generic, Eq)

instance FromJSON Scene where
  parseJSON :: Value -> Parser Scene
  parseJSON = worldParse
