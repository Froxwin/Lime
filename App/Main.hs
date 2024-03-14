{-# LANGUAGE DeriveDataTypeable #-}

module Main where


import           Engine                         ( ignite )
import           System.Console.CmdArgs         ( (&=)
                                                , Data
                                                , Default(def)
                                                , Typeable
                                                , cmdArgs
                                                , help
                                                , summary
                                                , typ
                                                )

data Lime = Lime
  { input  :: String
  , output :: String
  , preview  :: Bool
  }
  deriving (Show, Data, Typeable)

lime :: Lime
lime =
  Lime { input  = def &= help "input file" &= typ "INPUTFILE"
       , output = def &= help "output file" &= typ "OUTPUTFILE"
       , preview  = def &= help "render a preview image"
       }
    &= summary "Lime 0.1.0.0 (c) Froxwin"

main :: IO ()
main = do
  (Lime i o p) <- cmdArgs lime
  ignite i o p
