{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Camera
import           Color
import           Data.ByteString.Internal
import           Data.Yaml
import           Engine
import           Materials
import           Primitives
import           Test.Tasty
import           Test.Tasty.HUnit
import           Textures
import           Vector

main :: IO ()
main = defaultMain unitTests

unitTests :: TestTree
unitTests =
  testGroup "Tests" [cameraTests, objectTests, engineTests, parserTests]

mockScene :: Scene
mockScene = Scene
  { width     = 480
  , samples   = 10
  , height    = 270
  , maxBounce = 10
  , textures  = []
  , camera    = Camera { camUpwardVector    = Vector 0 1 0
                       , camLookingAt       = Vector 0 1 0
                       , camPosition        = Vector 0 1.2 (-1.3)
                       , camFieldOfView     = 1.57
                       , camFocalLength     = 1
                       , camDefocusAngle    = 0.08
                       , camBackgroundColor = Color 0 0 0
                       }
  , world     = [ Sphere { primCenter   = Vector 0 (-100.5) 0
                         , primRadius   = 100
                         , primMaterial = Lambertian $ SolidColor $ Color 1 0 0
                         }
                , Sphere
                  { primCenter   = Vector 0 1 0
                  , primRadius   = 1
                  , primMaterial = Metal
                                     { matFuzz    = 0.3
                                     , matTexture = Checkered 1
                                                              (Color 0 0 1)
                                                              (Color 0 1 0)
                                     }
                  }
                , Sphere { primCenter   = Vector 3 1 0
                         , primRadius   = 1
                         , primMaterial = Dielectric 1.5
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
      (all (all (<= 1) . (\(Color r g b) -> [r, g, b])) (render mockScene texs))
  , testCase "Correct Resolution" $ do
    texs <- loadTextures mockScene
    assertEqual "Checks if number of rendered pixels is correct"
                (length (render mockScene texs))
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

mockYAML :: ByteString
mockYAML =
  "\
\width: 480\n\
\height: 270\n\
\samples-per-pixel: 10\n\
\maximum-bounces: 10\n\
\texture-files: []\n\
\camera:\n\
\  upward-vector: [0, 1, 0]\n\
\  looking-at: [0, 1, 0]\n\
\  position: [0, 1.2, -1.3]\n\
\  field-of-view: 1.57\n\
\  focal-length: 1\n\
\  defocus-angle: 0.08\n\
\  background-color: [0, 0, 0]\n\
\world:\n\
\  - tag: Sphere\n\
\    center: [0, -100.5, 0]\n\
\    radius: 100\n\
\    material:\n\
\      tag: Lambertian\n\
\      texture:\n\
\        tag: SolidColor\n\
\        color: [255, 0, 0]\n\
\  - tag: Sphere\n\
\    center: [0, 1, 0]\n\
\    radius: 1\n\
\    material:\n\
\      tag: Metal\n\
\      fuzz: 0.3\n\
\      texture:\n\
\        tag: Checkered\n\
\        scale: 1\n\
\        color1: [0, 0, 255]\n\
\        color2: [0, 255, 0]\n\
\  - tag: Sphere\n\
\    center: [3, 1, 0]\n\
\    radius: 1\n\
\    material:\n\
\      tag: Dielectric\n\
\      ior: 1.5\n\
\"

parserTests :: TestTree
parserTests = testGroup
  "Parser Tests"
  [ testCase "Config Parsed" $ assertEqual
      "Checks if the yaml parser is working"
      (either (error . prettyPrintParseException) id (decodeEither' mockYAML)
      )
      mockScene
  ]
