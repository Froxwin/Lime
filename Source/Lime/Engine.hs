{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections       #-}

module Lime.Engine
  ( Scene
  , ignite
  , load
  , render
  ) where

import           Codec.Picture        (Image (Image, imageHeight, imageWidth),
                                       Pixel (colorMap, mixWith, pixelAt),
                                       PixelRGB8 (PixelRGB8),
                                       PixelRGBF (PixelRGBF), convertRGB8,
                                       generateImage, pixelMap, readImage,
                                       writeColorReducedGifImage, writePng)
-- import           Codec.Wavefront      (fromFile)
import           Control.DeepSeq      (deepseq)
import           Data.Map             (Map)
import qualified Data.Map             as M

import           Codec.Picture.Extra
import           Codec.Picture.Types
import           Data.Color           (Color (Color))
import qualified Data.Vector          as V
import qualified Data.Vector.Storable as VS
import           Lime.Camera          (Render (Render),
                                       Scene (height, models, textures, width),
                                       convertToPreview, render)
import           Lime.Internal.Utils  (prettyError)

import qualified Lime.Post
import           System.Random        (getStdGen)

import qualified Data.Massiv.Array    as A
import qualified Data.Massiv.Array.IO as A
import           Data.Word
import qualified Graphics.ColorModel  as GC

load
  :: FilePath
  -> (FilePath -> IO (Either String t))
  -> String
  -> Maybe [(String, FilePath)]
  -> IO (Map String t)
load dir f e =
  fmap M.fromList
    . maybe
      (return [])
      ( mapM (\(k, v) -> (k,) . either error id <$> f (concat [dir, "/", v]))
          . checkDupes
      )
 where
  checkDupes [] = []
  checkDupes ((a, b) : xs) =
    if a `elem` map fst xs
      then prettyError $ e ++ " `" ++ a ++ "` defined twice"
      else (a, b) : checkDupes xs

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
      <$> load texDir readImage "Texture" scene.textures
  -- !objs <- load modelDir fromFile "Object" scene.models

  gen <- getStdGen

  -- let (Render w h renderStream) = Lime.Post.bloom $ render gen scene texs objs
  -- let (Render w h renderStream) = render gen scene texs objs

  let pixelData = A.computeAs A.S $ (A.map (\t -> ((round :: Double -> Word8) . (* 255)) <$> t) $ render gen scene texs)

  -- let !pixelData =
  --       V.map
  --         ( \(Color r g b) ->
  --             PixelRGB8 (round $ r * 255) (round $ g * 255) (round $ b * 255)
  --         )
  --         renderStream

  -- let img =
  --       generateImage
  --         (\x y -> pixelData V.! ((w * y) + x))
  --         w
  --         h

  -- img `deepseq` writePng output img -- fully evaluate img before trying to write
  A.writeArray A.PNG A.def output pixelData
  putStrLn $ "[ \ESC[1;32mSaved to " ++ output ++ "\ESC[1;0m ]"
