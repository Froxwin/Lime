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
import           Vector                         ( Vector3(Vector3) )
import Data.List

objects :: [Object]
objects = [sphere (Vector3 0 (-100.5) 0) 100, sphere (Vector3 0 0.5 2) 1]

dropEvery :: Int -> [a] -> [a]
dropEvery _ [] = []
dropEvery n xs = take (n-1) xs ++ dropEvery n (drop n xs)

chunks :: Int -> [a] -> [[a]]
chunks n = takeWhile (not . null) . unfoldr (Just . splitAt n)

main :: IO ()
main = do
  (p, handle) <- openTempFile "." "temp.ppm"
  hPutStrLn handle
    $ concat ["P3\n", show imgWidth, " ", show imgHeight, " 255\n"]
  image <- render objects
  mapM_ (hPrint handle) image
  hClose handle
  putStrLn $ "[ \x1b[32mSaved to " ++ p ++ "\x1b[0m ]"
