{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}

module Lime.Engine
  ( Scene
  , ignite
  , loadAsset
  , render
  )
where

import Codec.Picture
import Control.DeepSeq (deepseq)
import Control.Lens (Field10 (_10))
import Data.Color (Color (Color), scale)
import Data.Fixed (mod')
import Data.List (minimumBy, nub, sort, (\\))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Ord (comparing)
import Data.Wavefront (loadWavefront)
import Lime.Camera
import Lime.Context
import Lime.Internal.Utils (prettyError)
import System.FilePath ((</>))
import System.Random (getStdGen)

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
          ( ( \(Color r g b) ->
                PixelRGB8 (round $ r * 255) (round $ g * 255) (round $ b * 255)
            )
              -- . toon
              . toneMap scene.config.exposure
          )
          -- \$ denoiseMedian3x3 scene.config.width scene.config.height
          $ render scene.config scene.camera (RenderCtx texs objs) scene.world gen

  let img =
        generateImage
          (\x y -> pixelData !! ((scene.config.width * y) + x))
          scene.config.width
          scene.config.height

  img `deepseq` writePng output img
  putStrLn $ "[ \ESC[1;32mSaved to " ++ output ++ "\ESC[1;0m ]"

-- Euclidean distance squared between colors
colorDistSq :: Color Double -> Color Double -> Double
colorDistSq (Color r1 g1 b1) (Color r2 g2 b2) =
  (r1 - r2) ^ 2 + (g1 - g2) ^ 2 + (b1 - b2) ^ 2

-- Convert flat list to 2D grid
toGrid :: Int -> [a] -> [[a]]
toGrid _ [] = []
toGrid w xs = take w xs : toGrid w (drop w xs)

-- Convert 2D grid back to flat list
fromGrid :: [[a]] -> [a]
fromGrid = concat

-- Get neighbors with bounds check
neighbors :: [[a]] -> Int -> Int -> [a]
neighbors grid y x =
  [ grid !! j !! i
  | j <- [y - 1 .. y + 1]
  , i <- [x - 1 .. x + 1]
  , j >= 0
  , i >= 0
  , j < length grid
  , i < length (head grid)
  ]

-- Find the "central" color: the one closest to all neighbors
centralColor :: [Color Double] -> Color Double
centralColor cs =
  minimumBy (comparing totalDist) cs
 where
  totalDist c = sum [colorDistSq c c' | c' <- cs]

-- Denoising kernel using the "most representative color"
denoise3x3 :: Int -> Int -> [Color Double] -> [Color Double]
denoise3x3 w h pixels =
  let grid = toGrid w pixels
      denoised =
        [ [ centralColor (neighbors grid y x)
          | x <- [0 .. w - 1]
          ]
        | y <- [0 .. h - 1]
        ]
   in fromGrid denoised

median :: [Double] -> Double
median xs =
  let s = sort xs
      n = length s
   in if odd n
        then s !! (n `div` 2)
        else (s !! (n `div` 2 - 1) + s !! (n `div` 2)) / 2

medianColor :: [Color Double] -> Color Double
medianColor cs =
  Color
    (median [r | Color r _ _ <- cs])
    (median [g | Color _ g _ <- cs])
    (median [b | Color _ _ b <- cs])

denoiseMedian3x3 :: Int -> Int -> [Color Double] -> [Color Double]
denoiseMedian3x3 w h pixels =
  let grid = toGrid w pixels
      denoised =
        [ [ medianColor (neighbors grid y x)
          | x <- [0 .. w - 1]
          ]
        | y <- [0 .. h - 1]
        ]
   in fromGrid denoised

toneMap :: (Functor f, Floating b) => b -> f b -> f b
toneMap k color = (\x -> 1 - exp ((-x) * k)) <$> color
