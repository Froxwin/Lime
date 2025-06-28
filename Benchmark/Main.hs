module Main where

import Control.Monad
import Criterion
import Criterion.Main
import System.Random

import Data.Color
import Data.Ray
import Lime.Camera
import Lime.Engine     (ignite)
import Lime.Materials  hiding (material)
import Lime.Primitives
import Lime.Textures
import Linear.V3

-- group :: Int -> [a] -> [[a]]
-- group _ [] = []
-- group n xs = take n xs : group n (drop n xs)

-- main :: IO ()
-- main = do
--   [rs1, rs2, rs3, rs4, rs5] <- mapM randomVals ([30, 60, 90, 120, 150] :: [Int])
--   defaultMain
--     [ bgroup
--         "Render tests"
--         [ bench "Primitives: 10" $ whnf (\t -> render (mockScene t) []) rs1
--         , bench "Primitives: 20" $ whnf (\t -> render (mockScene t) []) rs2
--         , bench "Primitives: 30" $ whnf (\t -> render (mockScene t) []) rs3
--         , bench "Primitives: 40" $ whnf (\t -> render (mockScene t) []) rs4
--         , bench "Primitives: 50" $ whnf (\t -> render (mockScene t) []) rs5
--         ]
--     ]
--  where
--   randomVals n =
--     replicateM n $ getStdRandom (randomR (-50 :: Double, 50 :: Double))
--   mockScene rs =
--     Scene
--       { width = 480
--       , height = 270
--       , samplesPerPixel = 10
--       , maximumBounces = 10
--       , textures = mempty
--       , models = mempty
--       , camera =
--           Camera
--             { upwardVector = V3 0 1 0
--             , lookingAt = V3 0 1 0
--             , position = V3 0 1.2 (-1.3)
--             , fieldOfView = 1.57
--             , focalLength = 1
--             , defocusAngle = 0.08
--             , backgroundTexture = SolidColor $ Color 0 0 0
--             }
--       , world =
--           map
--             ( ( \v ->
--                   Sphere
--                     { center = v
--                     , radius = 1
--                     , material = Lambertian $ SolidColor $ Color 255 0 0
--                     }
--               )
--                 . (\[x, y, z] -> V3 x y z)
--             )
--             (group 3 rs)
--       }

main :: IO ()
main = do
  defaultMain
    [ bgroup "Render tests" $
        map
          ( \k ->
              bench ("Samples: " <> show k) $
                whnf (\t -> render (saturn t) mempty mempty) k
          )
          [5, 10 .. 150]
    ]

saturn :: Double -> Scene
saturn sp =
  Scene
    { height = 255
    , width = 400
    , samplesPerPixel = sp
    , maximumBounces = 50
    , textures = mempty
    , models = mempty
    , camera =
        Camera
          { position = V3 0 0.6 (-2.5)
          , lookingAt = V3 0 0 0
          , focalLength = 1
          , fieldOfView = 1.57
          , upwardVector = V3 0 1 0
          , defocusAngle = 0
          , backgroundTexture = SolidColor $ Color 0 0 0
          }
    , world =
        [ Sphere
            { center = V3 15 5 (-25)
            , radius = 20
            , material = Emissive $ SolidColor $ Color 255 255 255
            }
        , Sphere
            { center = V3 0 0 0
            , radius = 1
            , material = Lambertian $ SolidColor $ Color 225 177 104
            }
        , Ring
            { center = V3 0 0 0
            , innerRadius = 1.7
            , outerRadius = 2
            , normal = V3 0.2 1 0.15
            , material = Lambertian $ SolidColor $ Color 255 226 174
            }
        , Ring
            { center = V3 0 0 0
            , innerRadius = 1.2
            , outerRadius = 1.5
            , normal = V3 0.2 1 0.15
            , material = Lambertian $ SolidColor $ Color 255 226 174
            }
        , Ring
            { center = V3 0 0 0
            , innerRadius = 2.3
            , outerRadius = 2.4
            , normal = V3 0.2 1 0.15
            , material = Lambertian $ SolidColor $ Color 255 226 174
            }
        ]
    }
