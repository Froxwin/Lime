module Engine where

import           Camera                         ( Scene(..)
                                                , Thing
                                                , render
                                                )
import           Data.List                      ( findIndex
                                                , findIndices
                                                , intercalate
                                                )
import           Data.Maybe                     ( Maybe
                                                , fromJust
                                                , isJust
                                                )
import           Data.Yaml                      ( decodeFileEither )
import           System.Directory               ( removeFile )
import           System.Environment             ( getArgs )
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
import Control.Exception (throw, Exception)
import System.Exit (die)

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

data Argument = Input | Output deriving (Show)

parseArgument :: String -> Maybe Argument
parseArgument arg | arg `elem` ["--input", "-i"]  = Just Input
                  | arg `elem` ["--output", "-o"] = Just Output
                  | otherwise                     = Nothing

parseArguments :: [String] -> [(Argument, String)]
parseArguments args = map
  (\x -> (fromJust $ parseArgument (args !! x), args !! (x + 1)))
  a
  where a = findIndices (isJust . parseArgument) args

data ArgumentException = InvalidArguments deriving (Show)

instance Exception ArgumentException

validateArguments :: [(Argument, String)] -> Config
validateArguments [(Input, a)] = Config a "./"
validateArguments [(Input, a), (Output, b)] = Config a b
validateArguments [(Output, b), (Input, a)] = Config a b
validateArguments _ = throw InvalidArguments

data Config = Config { inputPath :: String, outputPath :: String }

-- | Ignites the engine; renders the image -> makes ppm file -> calls ffmpeg to
--   make png -> removes ppm
ignite :: IO ()
ignite = do
  args <- getArgs
  let (Config i o) = validateArguments $ parseArguments args
  scene <- either (error . show) id <$> decodeFileEither i
  (p, handle) <- openTempFile "." "temp"
  hPutStr handle $ makeImageFile scene world
  hClose handle
  (_, _, _, ph) <- createProcess (proc "ffmpeg.exe" ["-i", p, o ++ ".png"])
  _             <- waitForProcess ph
  _             <- removeFile p
  putStrLn $ "[ \x1b[32mSaved to " ++ o ++ ".png" ++ "\x1b[0m ]"
