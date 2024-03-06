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
  , force  :: Bool
  }
  deriving (Show, Data, Typeable)

lime :: Lime
lime =
  Lime { input  = def &= help "input file" &= typ "INPUTFILE"
       , output = def &= help "output file" &= typ "OUTPUTFILE"
       , force  = def &= help "force overwrite file"
       }
    &= summary "Lime 0.1.0.0 (c) Froxwin"

main :: IO ()
main = do
  (Lime i o f) <- cmdArgs lime
  ignite i o f
