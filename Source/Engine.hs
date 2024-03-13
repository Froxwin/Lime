{-# LANGUAGE BangPatterns #-}

module Engine where

import           Camera                         ( Scene(height, width, textures)
                                                , render
                                                )
import           Codec.Picture
import           Color                          ( Color(Color) )
import           Data.Yaml                      ( decodeFileEither
                                                , prettyPrintParseException
                                                )
import Codec.Picture.Metadata (Value(Double))

group :: Int -> [a] -> [[a]]
group _ [] = []
group n xs = take n xs : group n (drop n xs)

loadTextures :: Scene -> IO [DynamicImage]
loadTextures scene = mapM (fmap (either error id) . readImage) $ textures scene

-- | Ignites the engine
ignite :: String -> String -> Bool -> IO ()
ignite input output _force = do
  !scene <- either (error . prettyPrintParseException) id
    <$> decodeFileEither input
  !texs <- loadTextures scene
  let !pixelData = group (width scene) $ render scene $ zip texs $ textures scene
  writePng output $ generateImage
    (\x y ->
      let (Color r g b) = ((pixelData !! y) !! x)
      in  PixelRGB8 (fromIntegral $ round $ r * 255)
                    (fromIntegral $ round $ g * 255)
                    (fromIntegral $ round $ b * 255)
    )
    (width scene)
    (height scene)
  putStrLn $ "[ \x1b[32mSaved to " ++ output ++ "\x1b[0m ]"
