module Engine where

import           Camera                         ( Scene(..)
                                                , Thing
                                                , render
                                                )
import           Data.List                      ( intercalate )
import           Data.Yaml                      ( decodeFileEither )
import           System.Directory               ( removeFile )
import           System.IO                      ( hClose
                                                , hPutStr
                                                , openTempFile
                                                )
import           System.Process                 ( createProcess
                                                , proc
                                                , waitForProcess
                                                )
import           Things.Sphere                  ( sphere )
import           Vector                         ( Vector(Vector) )

-- | List of all objects in world
world :: [Thing]
world =
  [ sphere (Vector 0 (-100.5) 0) 100
  , sphere (Vector 0 1 0)        1
  , sphere (Vector 3 1 0)        1
  ]

-- | Generates content for the ppm file
makeImageFile :: Scene -> [Thing] -> [Char]
makeImageFile scene objects =
  concat ["P3\n", show (width scene), " ", show (height scene), " 255\n"]
    ++ intercalate "\n" (map show $ render scene objects)

-- | Ignites the engine; renders the image -> makes ppm file -> calls ffmpeg to
--   make png -> removes ppm
ignite :: IO ()
ignite = do
  scene       <- either (error . show) id <$> decodeFileEither "./Examples/Scene.yaml"
  (p, handle) <- openTempFile "." "temp"
  hPutStr handle $ makeImageFile scene world
  hClose handle
  (_, _, _, ph) <- createProcess (proc "ffmpeg.exe" ["-i", p, p ++ ".png"])
  _             <- waitForProcess ph
  _             <- removeFile p
  putStrLn $ "[ \x1b[32mSaved to " ++ p ++ ".png" ++ "\x1b[0m ]"
