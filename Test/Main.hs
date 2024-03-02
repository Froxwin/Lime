import           Test.Tasty
import           Test.Tasty.HUnit

import           Camera
import           Engine
import           Ray
import           Things.Sphere
import           Things.Types
import           Vector

main :: IO ()
main = defaultMain unitTests

unitTests :: TestTree
unitTests = testGroup "Tests" [cameraTests, objectTests, engineTests]

mockScene :: Scene
mockScene = Scene
  { width   = 480
  , samples = 10
  , height  = 270
  , camera  = Camera { upVector     = Vector 0 1 0
                     , lookingAt    = Vector 0 1 0
                     , position     = Vector 0 1.2 (-1.3)
                     , fov          = 1.57
                     , focalLength  = 1
                     , defocusAngle = 0.08
                     }
  , world   = [ Sphere (Vector 0 (-100.5) 0) 100
              , Sphere (Vector 0 1 0)        1
              , Sphere (Vector 3 1 0)        1
              ]
  }

cameraTests :: TestTree
cameraTests = testGroup
  "Camera Tests"
  [ testCase "Valid Render"
  $  all (all (<= 1) . (\(Color r g b) -> [r, g, b])) (render mockScene)
  @? "Checks if all color values are less than one"
  , testCase "Correct Resolution" $ assertEqual
    "Checks if number of rendered pixels is correct"
    (length (render mockScene))
    (480 * 270)
  ]

objectTests :: TestTree
objectTests = testGroup
  "Object Tests"
  [ testCase "Hitting Sphere"
    $   rayColor (Ray (Vector 0 0 0) (Vector 0 0 1)) [sphere (Vector 0 0 50) 10]
    @?= Color 0.5 0.5 0
  ]

engineTests :: TestTree
engineTests = testGroup
  "Engine Tests"
  [ let fileHeader =
          "P3\n"
            ++ show (width mockScene)
            ++ " "
            ++ show (height mockScene)
            ++ " 255\n"
    in  testCase "Valid File"
        $   take (length fileHeader) (makeImageFile mockScene)
        @?= fileHeader
  ]
