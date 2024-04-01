{-# LANGUAGE BangPatterns #-}

module Engine where

import Camera        (Camera (camBackgroundColor), Scene (..), render)
import Codec.Picture (DynamicImage, PixelRGB8 (PixelRGB8), generateImage,
                      readImage, writePng)
import Color         (Color (Color))
import Data.Yaml     (decodeFileEither, prettyPrintParseException)
import Materials     (WorldMaterial (Emissive, Lambertian))
import Primitives    (WorldObject (primMaterial))
import Textures      (WorldTexture (SolidColor))

group :: Int -> [a] -> [[a]]
group _ [] = []
group n xs = take n xs : group n (drop n xs)

loadTextures :: Scene -> IO [(String, DynamicImage)]
loadTextures =
  maybe
      (return [])
      (mapM
        (\(k, v) -> do
          a <- either error id <$> readImage v
          return (k, a)
        )
      )
    . textures

getPix :: [Color] -> Int -> Int -> Int -> Color
getPix img w x y = img !! ((w * y) + x)

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
  !texs <- loadTextures scene
  let !pixelData = group (width scene) $ render scene texs
  writePng output $ generateImage
    (\x y ->
      let (Color r g b) = ((pixelData !! y) !! x)
      in  PixelRGB8 (fromIntegral $ round $ r * 255)
                    (fromIntegral $ round $ g * 255)
                    (fromIntegral $ round $ b * 255)
    )
    (width scene)
    (height scene)
  putStrLn $ "[ \ESC[1;32mSaved to " ++ output ++ "\ESC[1;0m ]"
