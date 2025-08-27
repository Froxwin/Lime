{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine
  ( Scene
  , ignite
  ) where

import Codec.Picture   (Pixel8, PixelRGB8 (PixelRGB8), generateImage, readImage,
                        writePng)
import Control.DeepSeq (deepseq)
import Data.Vector     ((!))

import Camera          (Camera (backgroundTexture), Scene (..), render)
import Color           (Color (Color), colorDouble2Word)
import Materials       (WorldMaterial (Emissive, Lambertian))
import Primitives      (WorldObject (material))
import Textures        (WorldTexture (SolidColor))
import Utils           (load, makeImage)

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
              _ -> x {material = Lambertian (SolidColor (Color 0.6 0.6 0.6))}
          )
          scene.world
    }

ignite :: FilePath -> Bool -> FilePath -> FilePath -> Scene -> IO ()
ignite output preview texDir modelDir rawScene = do
  let !scene = if preview then convertToPreview rawScene else rawScene
  !texs <- load texDir readImage "Texture" scene.textures
  let pixelData = makeImage scene.width $ render scene texs
  pixelData `deepseq`
    writePng output $
      generateImage
        ( \x y ->
            let Color r g b =
                  colorDouble2Word ((pixelData ! y) ! x) :: Color Pixel8
             in PixelRGB8 r g b
        )
        scene.width
        scene.height
  putStrLn $ "[ \ESC[1;32mSaved to " ++ output ++ "\ESC[1;0m ]"
