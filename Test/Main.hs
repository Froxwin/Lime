{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

module Main where

import Codec.Picture
import Codec.Wavefront
import Data.ByteString.Internal
import Data.Yaml
import Test.Tasty
import Test.Tasty.HUnit

import Camera
import Color
import Engine
import Materials                hiding (material)
import Primitives
import Textures                 hiding (texture)
import Utils
import Vector

main :: IO ()
main = defaultMain unitTests

unitTests :: TestTree
unitTests =
  testGroup "Tests" [cameraTests, objectTests, engineTests, parserTests]

cameraTests :: TestTree
cameraTests =
  testGroup
    "Camera Tests"
    [ testCase "Valid Render" $ do
        texs <- load "." readImage "Texture" mockScene.textures
        objs <- load "." fromFile "Object" mockScene.objects
        assertBool
          "Checks if generated pixel values are valid"
          ( all
              (all (<= 1) . (\(Color r g b) -> [r, g, b]))
              (render mockScene objs texs)
          )
    , testCase "Correct Resolution" $ do
        texs <- load "." readImage "Texture" mockScene.textures
        objs <- load "." fromFile "Object" mockScene.objects
        assertEqual
          "Checks if number of rendered pixels is correct"
          (length (render mockScene objs texs))
          (480 * 270)
    ]

objectTests :: TestTree
objectTests = testGroup "Object Tests" []

engineTests :: TestTree
engineTests =
  testGroup
    "Engine Tests"
    [ testCase "Textures Read" $ do
        texs <- load "." readImage "Texture" mockScene.textures
        assertEqual "Textures not read" 0 (length texs)
    ]

mockScene :: Scene
mockScene =
  Scene
    { width = 480
    , samplesPerPixel = 10
    , height = 270
    , maximumBounces = 10
    , textures = mempty
    , objects = mempty
    , camera =
        Camera
          { upwardVector = Vec3 0 1 0
          , lookingAt = Vec3 0 1 0
          , position = Vec3 0 1.2 (-1.3)
          , fieldOfView = 1.57
          , focalLength = 1
          , defocusAngle = 0.08
          , backgroundTexture = SolidColor $ Color 0 0 0
          }
    , world =
        [ Sphere
            { center = Vec3 0 (-100.5) 0
            , radius = 100
            , material = Lambertian $ SolidColor $ Color 1 0 0
            }
        , Sphere
            { center = Vec3 0 1 0
            , radius = 1
            , material =
                Metal
                  { fuzz = 0.3
                  , texture =
                      Checkered
                        1
                        (Color 0 0 1)
                        (Color 0 1 0)
                  }
            }
        , Sphere
            { center = Vec3 3 1 0
            , radius = 1
            , material = Dielectric 1.5
            }
        ]
    }

mockYAML :: ByteString
mockYAML =
  "\
  \width: 480\n\
  \height: 270\n\
  \samples_per_pixel: 10\n\
  \maximum_bounces: 10\n\
  \camera:\n\
  \  upward_vector: [0, 1, 0]\n\
  \  looking_at: [0, 1, 0]\n\
  \  position: [0, 1.2, -1.3]\n\
  \  field_of_view: 1.57\n\
  \  focal_length: 1\n\
  \  defocus_angle: 0.08\n\
  \  background_texture:\n\
  \    tag: solid-color\n\
  \    color: [0,0,0]\n\
  \world:\n\
  \  - tag: sphere\n\
  \    center: [0, -100.5, 0]\n\
  \    radius: 100\n\
  \    material:\n\
  \      tag: lambertian\n\
  \      texture:\n\
  \        tag: solid-color\n\
  \        color: [255, 0, 0]\n\
  \  - tag: sphere\n\
  \    center: [0, 1, 0]\n\
  \    radius: 1\n\
  \    material:\n\
  \      tag: metal\n\
  \      fuzz: 0.3\n\
  \      texture:\n\
  \        tag: checkered\n\
  \        scale: 1\n\
  \        color1: [0, 0, 255]\n\
  \        color2: [0, 255, 0]\n\
  \  - tag: sphere\n\
  \    center: [3, 1, 0]\n\
  \    radius: 1\n\
  \    material:\n\
  \      tag: dielectric\n\
  \      ior: 1.5\n\
  \"

parserTests :: TestTree
parserTests =
  testGroup
    "Parser Tests"
    [ testCase "Config Parsed" $
        assertEqual
          "Checks if the yaml parser is working"
          (either (error . prettyPrintParseException) id (decodeEither' mockYAML))
          mockScene
    ]
