{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Concurrent
  ( MVar
  , ThreadId
  , forkIO
  , modifyMVar
  , newEmptyMVar
  , newMVar
  , putMVar
  , takeMVar
  , tryTakeMVar
  )
import Control.Exception (try)
import Data.Map qualified as M
import Data.Yaml
  ( decodeFileEither
  , prettyPrintParseException
  )
import HsLua
  ( Exception
  , LuaE
  , LuaError
  , Name
  , Peekable (..)
  , Peeker
  , Type
  , dofile
  , getglobal
  , openlibs
  , peek
  , peekViaJSON
  , run
  , top
  )
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

import Lime.Engine (Scene, ignite)

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

instance Peekable Scene where
  safepeek :: LuaError e => Peeker e Scene
  safepeek = peekViaJSON

getExt :: String -> String
getExt =
  dropWhile (/= '.') . reverse . takeWhile (`notElem` ['/', '\\']) . reverse

main :: IO ()
main = do
  (Lime i o p t m fil) <- execParser lime
  let f = if null fil then Nothing else Just fil
  case getExt i of
    ".yaml" ->
      decodeFileEither i
        >>= ignite o p t m f
          . either
            ( errorWithoutStackTrace
                . ("\ESC[1;31m" <>)
                . (<> "\ESC[0m")
                . prettyPrintParseException
            )
            id
    ".lua" ->
      run
        ( openlibs
            >> dofile (Just i)
            >> (getglobal "scene" :: LuaE Exception Type)
            >> peek top
        )
        >>= ignite o p t m f
    ".anim.lua" -> do
      mn <- newManager
      run
        ( openlibs
            >> dofile (Just i)
            >> (getglobal "scenes" :: LuaE Exception Type)
            >> peek top
        )
        >>= mapM_
          ( \(j, s) ->
              forkManaged mn (ignite (o <> "/" <> show j <> ".png") p t m f s)
          )
          . (\xs -> zip [1 .. length xs] xs)
      waitAll mn
    _ -> errorWithoutStackTrace "\ESC[1;31mInvalid scene format\ESC[0m"

data ThreadStatus
  = Running
  | Finished
  | Threw !Exception
  deriving (Eq, Show)

newtype ThreadManager
  = Mgr (MVar (M.Map ThreadId (MVar ThreadStatus)))
  deriving Eq

newManager :: IO ThreadManager
newManager = Mgr <$> newMVar M.empty

forkManaged :: ThreadManager -> IO () -> IO ThreadId
forkManaged (Mgr mgr) body = modifyMVar mgr $ \m -> do
  state <- newEmptyMVar
  tid <- forkIO $ do
    result <- try body
    putMVar state (either Threw (const Finished) result)
  return (M.insert tid state m, tid)

getStatus :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
getStatus (Mgr mgr) tid = modifyMVar mgr $ \m -> case M.lookup tid m of
  Nothing -> return (m, Nothing)
  Just st ->
    tryTakeMVar st >>= \case
      Nothing -> return (m, Just Running)
      Just sth -> return (M.delete tid m, Just sth)

waitFor :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
waitFor (Mgr mgr) tid = do
  maybeDone <- modifyMVar mgr $ \m ->
    return $ case M.updateLookupWithKey (\_ _ -> Nothing) tid m of
      (Nothing, _) -> (m, Nothing)
      (done, m') -> (m', done)
  case maybeDone of
    Nothing -> return Nothing
    Just st -> Just <$> takeMVar st

waitAll :: ThreadManager -> IO ()
waitAll (Mgr mgr) = modifyMVar mgr elems >>= mapM_ takeMVar
 where
  elems m = return (M.empty, M.elems m)
