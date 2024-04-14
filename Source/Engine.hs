{-# LANGUAGE BangPatterns #-}

module Engine where

import Codec.Picture   (PixelRGB8 (PixelRGB8), generateImage, readImage,
                        writePng)
import Codec.Wavefront (fromFile)
import Data.Yaml       (decodeFileEither, prettyPrintParseException)

import Camera          (Camera (camBackgroundColor), Scene (..), render)
import Color           (Color (..))
import Materials       (WorldMaterial (Emissive, Lambertian))
import Primitives      (WorldObject (primMaterial))
import Textures        (WorldTexture (SolidColor))
import Utils           (group, load)

convertToPreview :: Scene -> Scene
convertToPreview scene = scene
  { height          = 255
  , width           = 400
  , samplesPerPixel = 50
  , maximumBounces  = 2
  , camera          = (camera scene) { camBackgroundColor = Color 1 1 1 }
  , world           = map
    (\x -> case primMaterial x of
      Emissive _ -> x
      _ -> x { primMaterial = Lambertian (SolidColor (Color 0.6 0.6 0.6)) }
    )
    (world scene)
  }

ignite :: FilePath -> FilePath -> Bool -> IO ()
ignite input output preview = do
  !scene <-
    (\x -> if preview then convertToPreview x else x)
    .   either (error . prettyPrintParseException) id
    <$> decodeFileEither input
  !texs <- load readImage "Texture" (textures scene)
  !objs <- load fromFile "Object" (objects scene)
  let !pixelData = group (width scene) $ render scene objs texs
  writePng output $ generateImage
    (\x y ->
      let (Color r g b) =
            fromIntegral . round . (* 255) <$> ((pixelData !! y) !! x)
      in  PixelRGB8 r g b
    )
    (width scene)
    (height scene)
  putStrLn $ "[ \ESC[1;32mSaved to " ++ output ++ "\ESC[1;0m ]"
