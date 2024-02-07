module Main where

import           Camera                         ( Object
                                                , imgHeight
                                                , imgWidth
                                                , render
                                                )
import           Objects.Sphere                 ( sphere )
import           System.IO                      ( hClose
                                                , hPrint
                                                , hPutStrLn
                                                , openTempFile
                                                )
import           Vector                         ( Vector(Vector) )

objects :: [Object]
objects = [sphere (Vector 0 (-100.5) 0) 100, sphere (Vector 0 0.5 2) 1]

main :: IO ()
main = do
  (p, handle) <- openTempFile "." "temp.ppm"
  hPutStrLn handle
    $ concat ["P3\n", show imgWidth, " ", show imgHeight, " 255\n"]
  mapM_ (hPrint handle) $ render objects
  hClose handle
  putStrLn $ "[ \x1b[32mSaved to " ++ p ++ "\x1b[0m ]"
