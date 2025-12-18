{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Lime.Camera where

import Codec.Picture
import Control.DeepSeq (deepseq, force)
import Control.Parallel.Strategies
import Data.Aeson.Types
import Data.Color
import Data.List
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Ray
import Data.Wavefront qualified
import GHC.Generics
import Lime.Context
import Lime.Internal.Hit
import Lime.Internal.Utils
import Lime.Primitives
import Lime.Textures
import Linear
import System.Random

integrate
  :: RenderCtx
  -> Ray
  -> BVH
  -> V3 Float
  -> Int
  -> Texture
  -> (Maybe HitData, Color Double)
integrate ctx ray bvh g depth background = (fst <$> maybeHit, rayColor ctx ray bvh g depth background)
 where
  maybeHit = traverseBVH bvh ray 1.0e-3 (1 / 0)

rayColor
  :: RenderCtx
  -> Ray
  -> BVH
  -> V3 Float
  -> Int
  -> Texture
  -> Color Double
rayColor ctx ray@(Ray _ dir) bvh g depth background
  | depth == 0 = pure 0
  | isNothing maybeHit =
      background
        ( HitData
            { coords =
                (\(V3 px py pz) -> ((atan2 (-pz) px + pi) / (2 * pi), acos (-py) / pi)) dir
            , point = dir
            , normal = error "Tried to access skybox normal"
            , param = error "Tried to access skybox hit"
            }
        )
  | otherwise =
      maybe
        color
        (getColorProduct . (ColorProduct color <>) . ColorProduct)
        reflectedColor
 where
  reflectedColor = (\t -> rayColor ctx t bvh g (depth - 1) background) <$> reflected
  (color, reflected) = material g
  (_, material) = fromJust maybeHit
  maybeHit = traverseBVH bvh ray 1.0e-3 (1 / 0)

render
  :: SceneConfig
  -> Camera
  -> RenderCtx
  -> [Lime.Camera.Object]
  -> StdGen
  -> [Color Double]
render (SceneConfig {..}) (Camera {..}) ctx world gen =
  let primitives =
        force $
          concatMap
            ( \q -> case q of
                Single s -> [s]
                Compund (Mesh s t m) ->
                  map
                    (\z -> Shape z t m)
                    (triangulate (ctx.models M.! s))
            )
            world
      !bvh = force $ constructBVH ctx primitives
      bg = force $ texture ctx backgroundTexture
      pixelColor rgen (ui, vi) =
        ( gen'
        , ( \(hs, cs) ->
              let c =
                    getColorSum $
                      mconcat $
                        map
                          ( ColorSum . \x -> if depthEdge hs then (*) <$> (Color 0.38 0.24 0.30) <*> x else x
                          )
                          cs
               in getColorSum $ mconcat $ map ColorSum cs
          )
            $ unzip
              [ let (hit, color) =
                      integrate
                        ctx
                        (Ray diskSample (normalize (pixelSample ^-^ diskSample)))
                        bvh
                        r
                        maximumBounces
                        bg
                 in (hit, (1 / samplesPerPixel) `scale` color)
              | (V2 sx sy, V2 dx dy, r) <- ss
              , let pixelSample =
                      bottomLeft
                        ^+^ ((ui + (sx / fromIntegral width)) *^ horizontal)
                        ^+^ ((vi + (sy / fromIntegral height)) *^ vertical)
              , let diskSample =
                      position ^+^ (dx *^ diskU) ^+^ (dy *^ diskV)
              ]
        )
       where
        (gen0, sampleGen) = split rgen
        (gen1, diskGen) = split gen0
        (gen', scatterGen) = split gen1

        sampleSquare :: [V2 Float]
        sampleSquare =
          take (round samplesPerPixel) $
            randomRs (V2 (-0.5) (-0.5), V2 0.5 0.5) sampleGen

        bokeh :: V2 Float -> Bool
        bokeh (V2 x y) = ((x' ^ 2) + (y' ^ 2) - 0.1) ^ 3 <= (x' ^ 2) * (y' ^ 3)
         where
          x' = a * x
          y' = a * (y - k)
          a = 0.8
          k = -0.1

        depthEdge :: [Maybe HitData] -> Bool
        depthEdge hs =
          let hits = [h | Just h <- hs]
              hitCount = length hits
              missCount = length hs - hitCount
              total = length hs

              ratio =
                fromIntegral (min hitCount missCount)
                  / fromIntegral total

              depthJump =
                case hits of
                  (_ : _ : _) ->
                    let ts = map (\(HitData _ _ t _) -> t) hits
                        t̄ = sum ts / fromIntegral (length ts)
                     in maximum ts - minimum ts > 0.018 * t̄
                  _ -> False
           in ratio > 0.005 || depthJump

        sampleDisk :: [V2 Float]
        sampleDisk =
          take (round samplesPerPixel) $
            filter bokeh $
              randomRs (V2 (-0.5) (-0.5), V2 0.5 0.5) diskGen

        randomVecs :: [V3 Float]
        randomVecs =
          map normalize $
            take (round samplesPerPixel) $
              randomRs (V3 (-1) (-1) (-1), V3 1 1 1) scatterGen

        ss = zip3 sampleSquare sampleDisk randomVecs

      jobs = max 1 ((width * height) `div` (16 * 20))
      coords =
        [ (i, j)
        | j <- reverse $ scanline $ fromIntegral height
        , i <- scanline $ fromIntegral width
        ]
   in bvh
        `deepseq` withStrategy
          (parListChunk jobs rdeepseq)
          (snd $ mapAccumL pixelColor gen coords)
 where
  scanline q = map (/ q) [0 .. q - 1]

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
    position
      ^-^ (horizontal ^/ 2)
      ^-^ (vertical ^/ 2)
      ^-^ (focalLength *^ w)

  -- aperture
  diskRadius = focalLength * tan (defocusAngle / 2)
  diskU = diskRadius *^ u
  diskV = diskRadius *^ v

-- tosimd2 (V2 x y) = pack (x, y)
-- tosimd3 (V3 x y z) = pack (x, y, z)

convertToPreview :: Scene -> Scene
convertToPreview scene =
  scene
    { config =
        scene.config
          { height = 90
          , width = 160
          , samplesPerPixel = 8
          , maximumBounces = 2
          }
    , camera = scene.camera {backgroundTexture = SolidColor (Color 1 1 1)}
    }

data Camera = Camera
  { position :: !(V3 Float)
  , lookingAt :: !(V3 Float)
  , focalLength :: !Float
  , fieldOfView :: !Float
  , upwardVector :: !(V3 Float)
  , defocusAngle :: !Float
  , backgroundTexture :: !TextureNode
  }
  deriving (Show, Generic, Eq)

instance FromJSON Camera where
  parseJSON = worldParse

data Object = Single Shape | Compund Mesh deriving (Show, Generic, Eq)

instance FromJSON Lime.Camera.Object where
  parseJSON = worldParse

data Scene = Scene
  { config :: !SceneConfig
  , camera :: !Camera
  , textures :: Maybe [(String, FilePath)]
  , models :: Maybe [(String, FilePath)]
  , world :: ![Lime.Camera.Object]
  }
  deriving (Show, Generic, Eq)

instance FromJSON Scene where
  parseJSON = worldParse

data SceneConfig = SceneConfig
  { width :: !Int
  , height :: !Int
  , samplesPerPixel :: !Double
  , maximumBounces :: !Int
  , exposure :: !Double
  }
  deriving (Show, Generic, Eq)

instance FromJSON SceneConfig where
  parseJSON = worldParse
