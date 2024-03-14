{-# LANGUAGE BangPatterns #-}

module Engine where

import           Camera                         ( Scene(height, textures, width)
                                                , render
                                                )
import           Codec.Picture                  ( DynamicImage
                                                , PixelRGB8(PixelRGB8)
                                                , generateImage
                                                , readImage
                                                , writePng
                                                )
import           Color                          ( Color(Color) )
import           Data.Yaml                      ( decodeFileEither
                                                , prettyPrintParseException
                                                )

group :: Int -> [a] -> [[a]]
group _ [] = []
group n xs = take n xs : group n (drop n xs)

loadTextures :: Scene -> IO [(String, DynamicImage)]
loadTextures scene = mapM loadTexture $ textures scene
 where
  loadTexture (k, v) = do
    a <- either error id <$> readImage v
    return (k, a)

ignite :: String -> String -> Bool -> IO ()
ignite input output _preview = do
  !scene <- either (error . prettyPrintParseException) id
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
  putStrLn $ "[ \x1b[32mSaved to " ++ output ++ "\x1b[0m ]"
