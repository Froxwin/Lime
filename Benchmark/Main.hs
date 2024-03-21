module Main where

import           Control.Monad
import           Criterion
import           Criterion.Main
import           System.Random

import           Camera
import           Color
import           Engine                         ( ignite )
import           Materials
import           Primitives
import           Ray
import           Textures
import           Vector

group :: Int -> [a] -> [[a]]
group _ [] = []
group n xs = take n xs : group n (drop n xs)

main :: IO ()
main = do
  [rs1, rs2, rs3, rs4, rs5] <- mapM randomVals [30, 60, 90, 120, 150]
  defaultMain
    [ bgroup
        "Render tests"
        [ bench "Primitives: 10" $ whnf (\t -> render (mockScene t) []) rs1
        , bench "Primitives: 20" $ whnf (\t -> render (mockScene t) []) rs2
        , bench "Primitives: 30" $ whnf (\t -> render (mockScene t) []) rs3
        , bench "Primitives: 40" $ whnf (\t -> render (mockScene t) []) rs4
        , bench "Primitives: 50" $ whnf (\t -> render (mockScene t) []) rs5
        ]
    ]
 where
  randomVals n =
    replicateM n $ getStdRandom (randomR (-50 :: Double, 50 :: Double))
  mockScene rs = Scene
    { width     = 480
    , samples   = 10
    , height    = 270
    , maxBounce = 10
    , textures  = []
    , camera    = Camera { upVector     = Vector 0 1 0
                         , lookingAt    = Vector 0 1 0
                         , position     = Vector 0 1.2 (-1.3)
                         , fov          = 1.57
                         , focalLength  = 1
                         , defocusAngle = 0.08
                         , defaultColor = Color 0 0 0
                         }
    , world     = map
                    ( (\v -> Sphere
                        { center   = v
                        , radius   = 1
                        , material = Lambertian $ SolidColor $ Color 255 0 0
                        }
                      )
                    . (\[x, y, z] -> Vector x y z)
                    )
                    (group 3 rs)
    }
