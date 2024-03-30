{-# LANGUAGE DeriveDataTypeable #-}

module Main where


import           Engine                 (ignite)
import           System.Console.CmdArgs (Data, Default (def), Typeable, cmdArgs,
                                         help, summary, typFile, (&=))

data Lime = Lime
  { input   :: FilePath
  , output  :: FilePath
  , preview :: Bool
  }
  deriving (Show, Data, Typeable)

lime :: Lime
lime =
  Lime { input   = def &= help "File with scene to be rendered" &= typFile
       , output  = def &= help "Output image file" &= typFile
       , preview = def &= help "Whether to render a preview image"
       }
    &= summary "Lime 0.1.0.0 (c) Froxwin"

main :: IO ()
main = do
  (Lime i o p) <- cmdArgs lime
  if i == "" || o == ""
    then errorWithoutStackTrace "\ESC[31mProvide valid arguments\ESC[0m"
    else ignite i o p
