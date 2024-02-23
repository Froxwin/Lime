module Engine where

import           Camera                         ( Scene(..)
                                                , Thing
                                                , render
                                                )
import           Data.Yaml                      ( decodeFileEither )
import           System.IO                      ( hClose
                                                , hPrint
                                                , hPutStrLn
                                                , openTempFile
                                                )
import           Things.Sphere                  ( sphere )
import           Vector                         ( Vector(Vector) )

-- | List of all objects in world
objects :: [Thing]
objects = [sphere (Vector 0 (-100.5) 0) 100, sphere (Vector 0 1 0) 1]

readScene :: IO Scene
readScene = either (error . show) id <$> decodeFileEither "./scene.yaml"

ignite :: IO ()
ignite = do
  scene       <- readScene
  (p, handle) <- openTempFile "." "temp.ppm"
  hPutStrLn handle
    $ concat ["P3\n", show (width scene), " ", show (height scene), " 255\n"]
  mapM_ (hPrint handle) $ render scene objects
  hClose handle
  putStrLn $ "[ \x1b[32mSaved to " ++ p ++ "\x1b[0m ]"
