{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Codec.Picture
import Control.Concurrent (threadDelay)
import Control.DeepSeq (deepseq)
import Control.Monad
import Data.ByteString qualified as B
import Data.ByteString.Internal qualified as B
import Data.Color
import Data.Color (Color (Color), scale)
import Data.Fixed (mod')
import Data.List (minimumBy, nub, sort, (\\))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Ord (comparing)
import Data.Vector qualified as VV
import Data.Vector.Storable qualified as VS
import Data.Vector.Strict qualified as V
import Data.Wavefront (Object, loadWavefront)
import Data.Word (Word8)
import Data.Yaml
  ( decodeFileEither
  , prettyPrintParseException
  )
import Foreign.C
import Foreign.C.String (peekCString)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Safe (pokeArray)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Safe (plusPtr)
import Lime.Camera
import Lime.Context
import Lime.Engine (Scene, ignite)
import Lime.Internal.Utils (prettyError)
import Options.Applicative
  ( ParserInfo
  , execParser
  , fullDesc
  , header
  , help
  , helper
  , info
  , long
  , metavar
  , progDesc
  , short
  , showDefault
  , strOption
  , switch
  , value
  , (<**>)
  )
import SDL
import SDL.Raw.Video qualified as SDL
import SDL.Video.Renderer
import System.FilePath ((</>))
import System.Random (getStdGen)

data Lime = Lime
  { input :: !FilePath
  , output :: !FilePath
  , preview :: !Bool
  , textures :: !FilePath
  , models :: !FilePath
  , filter :: !FilePath
  }

lime :: ParserInfo Lime
lime =
  info
    ( ( Lime
          <$> strOption
            ( long "input"
                <> short 'i'
                <> metavar "FILE"
                <> help
                  "YAML/Lua scene file"
            )
          <*> strOption
            ( long "output"
                <> short 'o'
                <> value "out.png"
                <> showDefault
                <> metavar "FILE"
                <> help "Output image"
            )
          <*> switch
            ( long "preview"
                <> short 'p'
                <> help
                  "Whether to render a preview image"
            )
          <*> strOption
            ( long "textures"
                <> short 't'
                <> value "."
                <> showDefault
                <> metavar "DIR"
                <> help "Directory with texture files"
            )
          <*> strOption
            ( long "models"
                <> short 'm'
                <> value "."
                <> showDefault
                <> metavar "DIR"
                <> help "Directory with model files"
            )
          <*> strOption
            (long "filter" <> value "" <> metavar "FILES" <> help "Lua filters")
      )
        <**> helper
    )
    (fullDesc <> header "Lime 0.1.0.0 (c) Froxwin" <> progDesc "A raytracer")

main :: IO ()
main = do
  (Lime i o p t m fil) <- execParser lime
  scene <-
    ( either
        ( errorWithoutStackTrace
            . ("\ESC[1;31m" <>)
            . (<> "\ESC[0m")
            . prettyPrintParseException
        )
        id
        <$> decodeFileEither i
    )
      :: IO Scene
  putStrLn "[ IGNITION ]"
  initializeAll
  window <-
    createWindow "Lanthanum" $
      defaultWindow {windowHighDPI = True, windowResizable = True}
  renderer <- createRenderer window (-1) defaultRenderer
  videoDriver <- SDL.getCurrentVideoDriver >>= peekCString
  putStrLn $ "Video Driver: " ++ videoDriver

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
      <$> loadAsset "./" readImage "Texture" scene.textures
  !objs <- loadAsset "./" loadWavefront "Model" scene.models
  texture <-
    createTexture
      renderer
      RGB24 -- pixel format matches your surface
      TextureAccessStatic -- static: we’ll update once
      (V2 (fromIntegral scene.config.width) (fromIntegral scene.config.height))

  appLoop renderer texture texs objs scene
  destroyWindow window
  putStrLn "[ FIN ]"

appLoop
  :: Renderer
  -> Texture
  -> (M.Map String (Image PixelRGBF))
  -> (M.Map String Data.Wavefront.Object)
  -> Scene
  -> IO ()
appLoop renderer texture texs objs scene = do
  event <- pollEvent
  let eventIsQPress e =
        case eventPayload e of
          QuitEvent -> True
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed
              && keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False

      dirty e =
        case eventPayload e of
          KeyboardEvent k ->
            if keyboardEventKeyMotion k == Pressed
              then case keysymKeycode (keyboardEventKeysym k) of
                KeycodeW ->
                  Just
                    scene {camera = scene.camera {position = scene.camera.position ^-^ V3 0 0 0.3}}
                KeycodeS ->
                  Just
                    scene {camera = scene.camera {position = scene.camera.position ^+^ V3 0 0 0.3}}
                KeycodeA ->
                  Just
                    scene {camera = scene.camera {position = scene.camera.position ^-^ V3 0.2 0 0}}
                KeycodeD ->
                  Just
                    scene {camera = scene.camera {position = scene.camera.position ^+^ V3 0.2 0 0}}
                KeycodeE ->
                  Just
                    scene {camera = scene.camera {position = scene.camera.position ^+^ V3 0 0.2 0}}
                KeycodeR ->
                  Just
                    scene {camera = scene.camera {position = scene.camera.position ^-^ V3 0 0.2 0}}
                _ -> Nothing
              else Nothing
          _ -> Nothing
      qPressed = maybe False eventIsQPress event
      isdirty = maybe Nothing dirty event

  gen <- getStdGen

  case isdirty of
    Nothing -> do
      rendererDrawColor renderer $= V4 30 30 46 255
      clear renderer
      copy renderer texture Nothing Nothing
      present renderer
      unless qPressed (appLoop renderer texture texs objs scene)
    Just s' -> do
      let !pixelData =
            ( V.force
                $ V.concatMap
                  ( ( \(Color r g b) ->
                        V.fromList [(round $ r * 255), (round $ g * 255), (round $ b * 255)]
                    )
                      . toneMap scene.config.exposure
                  )
                $ V.force
                $ render scene.config s'.camera (RenderCtx texs objs) scene.world gen
            )
              :: V.Vector Word8

      let rawData = VS.convert pixelData

      let pitch = fromIntegral $ 3 * scene.config.width -- bytes per row
      let (fptr, offset, len) = VS.unsafeToForeignPtr rawData
      let byteStr = B.fromForeignPtr fptr offset len
      updateTexture texture Nothing byteStr (fromIntegral pitch)

      rendererDrawColor renderer $= V4 30 30 46 255
      clear renderer
      copy renderer texture Nothing Nothing
      present renderer

      unless qPressed (appLoop renderer texture texs objs s')

toneMap :: (Functor f, Floating b) => b -> f b -> f b
toneMap k color = (\x -> 1 - exp ((-x) * k)) <$> color

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
