{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Codec.Picture

-- import           Codec.Wavefront
import Data.ByteString.Internal
import Data.Maybe
import Data.Yaml
import Test.Tasty
import Test.Tasty.HUnit

import Data.Color
import Data.Ray
import Lime.Camera
import Lime.Engine
import Lime.Internal.Hit hiding (texture)
import Lime.Internal.Utils
import Lime.Materials hiding (material)
import Lime.Primitives
import Lime.Textures hiding (texture)
import Linear.V3

import Optics qualified
import Primitives qualified
import Transform qualified

main :: IO ()
main = defaultMain unitTests

unitTests :: TestTree
unitTests =
  testGroup "Tests" [Primitives.tests, Optics.tests, Transform.tests]

-- engineTests :: TestTree
-- engineTests =
--   testGroup
--     "Engine Tests"
--     [ testCase "Textures Read" $ do
--         texs <- load "." readImage "Texture" mockScene.textures
--         assertEqual "Textures not read" 0 (length texs)
--     ]

-- parserTests :: TestTree
-- parserTests =
--   testGroup
--     "Parser Tests"
--     [ testCase "Config Parsed" $
--         assertEqual
--           "Checks if the yaml parser config is working"
--           (either (error . prettyPrintParseException) id (decodeEither' mockYAML))
--           mockScene
--     ]

-- mockScene :: Scene
-- mockScene =
--   Scene
--     { width = 400
--     , height = 255
--     , samplesPerPixel = 100
--     , maximumBounces = 50
--     , textures = mempty
--     , models = mempty
--     , camera =
--         Camera
--           { upwardVector = V3 0 1 0
--           , lookingAt = V3 0 1 0
--           , position = V3 0 1.2 1.3
--           , fieldOfView = 1.57
--           , focalLength = 1
--           , defocusAngle = 0.08
--           , backgroundTexture = SolidColor $ Color 0 0 0
--           }
--     , world =
--         [ Sphere
--             { center = V3 0 (-100.5) 0
--             , radius = 100
--             , material = Lambertian $ SolidColor $ Color 1 0 0
--             }
--         , Sphere
--             { center = V3 0 1 0
--             , radius = 1
--             , material =
--                 Metal
--                   { fuzz = 0.3
--                   , texture =
--                       Checkered
--                         1
--                         (SolidColor $ Color 0 0 1)
--                         (SolidColor $ Color 0 1 0)
--                   }
--             }
--         , Sphere
--             { center = V3 3 1 0
--             , radius = 1
--             , material = Dielectric 1.5
--             }
--         ]
--     }

mockYAML :: ByteString
mockYAML =
  "\
  \width: 400\n\
  \height: 255\n\
  \samples_per_pixel: 100\n\
  \maximum_bounces: 50\n\
  \camera:\n\
  \  upward_vector: [0, 1, 0]\n\
  \  looking_at: [0, 1, 0]\n\
  \  position: [0, 1.2, 1.3]\n\
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
  \        texture1:\n\
  \          tag: solid-color\n\
  \          color: [0, 0, 255]\n\
  \        texture2:\n\
  \          tag: solid-color\n\
  \          color: [0, 255, 0]\n\
  \  - tag: sphere\n\
  \    center: [3, 1, 0]\n\
  \    radius: 1\n\
  \    material:\n\
  \      tag: dielectric\n\
  \      ior: 1.5\n\
  \"

-- mockMaterials =
--   [
--     ( Lambertian $ SolidColor $ Color 255 0 0
--     , "tag: lambertian\n\
--       \texture:\n\
--       \  tag: solid-color\n\
--       \"
--     )
--   , Metal 0 $ SolidColor $ Color 255 0 0
--   , Dielectric 1.7
--   , Emissive $ SolidColor $ Color 255 0 0
--   ]

-- mockTextures =
--   [ SolidColor $ Color 255 0 0
--   , Checkered 1 (SolidColor $ Color 100 50 0) (SolidColor $ Color 10 50 100)
--   , ImageTexture "placeholder" Stretch
--   ]

-- cameraTests :: TestTree
-- cameraTests =
-- testGroup
-- "Camera Tests"
-- [ testCase "Valid Render" $ do
--     texs <- load "." readImage "Texture" mockScene.textures
--     objs <- load "." fromFile "Object" mockScene.objects
--     assertBool
--       "Checks if generated pixel values are valid"
--       ( all
--           (all (<= 1) . (\(Color r g b) -> [r, g, b]))
--           (render mockScene objs texs)
--       )
--  testCase "Correct Resolution" $ do
--     texs <- load "." readImage "Texture" mockScene.textures
--     objs <- load "." fromFile "Object" mockScene.models
--     assertEqual
--       "Checks if number of rendered pixels is correct"
--       (length (render mockScene objs texs))
--       (480 * 270)
-- ]
