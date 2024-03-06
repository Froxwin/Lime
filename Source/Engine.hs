module Engine where

import           Camera                         ( Scene(height, width)
                                                , render
                                                )
import           Codec.Picture                  ( PixelRGB8(PixelRGB8)
                                                , generateImage
                                                , writePng
                                                )
import           Data.Yaml                      ( decodeFileEither
                                                , prettyPrintParseException
                                                )
import           Things.Types                   ( Color(Color) )

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l  = take n l : group n (drop n l)

-- | Ignites the engine
ignite :: String -> String -> Bool -> IO ()
ignite input output force = do
  scene <- either (error . prettyPrintParseException) id
    <$> decodeFileEither input
  let pixelData = group (width scene) $ render scene
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
