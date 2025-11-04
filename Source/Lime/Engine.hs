{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}

module Lime.Engine
  ( Scene
  , ignite
  , loadAsset
  , render
  ) where

import Codec.Picture
import Control.DeepSeq (deepseq)
import Data.Color (Color (Color))
import Data.List (nub, (\\))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Wavefront (loadWavefront)
import System.FilePath ((</>))
import System.Random (getStdGen)

import Lime.Camera
import Lime.Internal.Utils (prettyError)

-- | Function to load any kind of asset (textures, models , etc)
loadAsset
  :: FilePath
  -- ^ Directory Containing assets
  -> (FilePath -> IO (Either String t))
  -- ^ Loader function
  -> String
  -- ^ Docstring describing assets being loaded
  -> Maybe [(String, FilePath)]
  -- ^ List of asset keys and filenames
  -> IO (Map String t)
loadAsset dir f e =
  fmap M.fromList
    . maybe
      (return [])
      ( mapM (\(k, v) -> (k,) . either prettyError id <$> f (dir </> v))
          . checkDupes
      )
 where
  checkDupes xs =
    let keys = map fst xs
        dups = keys \\ nub keys
     in if null dups
          then xs
          else
            prettyError $
              "Asset loading failed:\n"
                ++ concatMap (\q -> e ++ " `" ++ q ++ "` defined twice\n") dups

type Filter = FilePath

ignite
  :: FilePath -> Bool -> FilePath -> FilePath -> Maybe Filter -> Scene -> IO ()
ignite output preview texDir modelDir fil rawScene = do
  let !scene = if preview then convertToPreview rawScene else rawScene
  !texs <-
    M.map
      ( pixelMap
          ( \(PixelRGB8 r g b) ->
              PixelRGBF
                (fromIntegral (toInteger r) / 255)
                (fromIntegral (toInteger g) / 255)
                (fromIntegral (toInteger b) / 255)
          )
          . convertRGB8
      )
      <$> loadAsset texDir readImage "Texture" scene.textures
  !objs <- loadAsset modelDir loadWavefront "Model" scene.models

  gen <- getStdGen

  let !pixelData =
        map
          ( \(Color r g b) ->
              PixelRGB8 (round $ r * 255) (round $ g * 255) (round $ b * 255)
          )
          $ render scene gen texs objs

  let img =
        generateImage
          (\x y -> pixelData !! ((scene.width * y) + x))
          scene.width
          scene.height

  img `deepseq` writePng output img
  putStrLn $ "[ \ESC[1;32mSaved to " ++ output ++ "\ESC[1;0m ]"
