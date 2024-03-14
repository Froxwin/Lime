import           Test.Tasty
import           Test.Tasty.HUnit

import           Camera
import           Color
import           Engine
import           Materials
import           Primitives
import           Ray
import           Textures
import           Vector

main :: IO ()
main = defaultMain unitTests

unitTests :: TestTree
unitTests = testGroup "Tests" [cameraTests, objectTests, engineTests]

mockScene :: Scene
mockScene = Scene
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
                       }
  , world     = [ Sphere
                  { center = Vector 0 (-100.5) 0
                  , radius = 100
                  , material = Lambertian { texture = SolidColor (Color 255 0 0) }
                  }
                , Sphere
                  { center   = Vector 0 1 0
                  , radius   = 1
                  , material = Metal
                                 { fuzz    = 0.3
                                 , texture = Checkered 1
                                                       (Color 0 0 255)
                                                       (Color 0 255 0)
                                 }
                  }
                , Sphere { center   = Vector 3 1 0
                         , radius   = 1
                         , material = Dielectric 1.5
                         }
                ]
  }

cameraTests :: TestTree
cameraTests = testGroup
  "Camera Tests"
  [ testCase "Valid Render" $ do
    texs <- loadTextures mockScene
    assertBool
      "Checks if generated pixel values are valid"
      (all (all (<= 1) . (\(Color r g b) -> [r, g, b])) (render mockScene []))
  , testCase "Correct Resolution" $ do
    texs <- loadTextures mockScene
    assertEqual "Checks if number of rendered pixels is correct"
                (length (render mockScene []))
                (480 * 270)
  ]

objectTests :: TestTree
objectTests = testGroup "Object Tests" []

engineTests :: TestTree
engineTests = testGroup
  "Engine Tests"
  [ testCase "Textures Read" $ do
      texs <- loadTextures mockScene
      assertEqual "Textures not read" 0 (length texs)
  ]
