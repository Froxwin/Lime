{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import           Control.Concurrent  (MVar, ThreadId, forkIO, modifyMVar,
                                      newEmptyMVar, newMVar, putMVar, takeMVar,
                                      tryTakeMVar)
import           Control.Exception   (try)
import qualified Data.Map            as M
import           Data.Yaml           (decodeFileEither,
                                      prettyPrintParseException)
import           HsLua               (Exception, LuaE, LuaError, Name,
                                      Peekable (..), Peeker, Type, dofile,
                                      getglobal, openlibs, peek, peekViaJSON,
                                      run, top)
import           Options.Applicative (ParserInfo, execParser, fullDesc, header,
                                      help, helper, info, long, metavar, short,
                                      strOption, switch, value, (<**>))

import           Engine              (Scene, ignite)

data Lime = Lime
  { input :: FilePath
  , output :: FilePath
  , preview :: Bool
  , textures :: FilePath
  , models :: FilePath
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
                  "YAML scene file"
            )
          <*> strOption
            ( long "output"
                <> short 'o'
                <> value "out.png"
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
                <> metavar "DIR"
                <> help
                  "Directory with texture files"
            )
          <*> strOption
            ( long "models"
                <> short 'm'
                <> value "."
                <> metavar "DIR"
                <> help
                  "Directory with model files"
            )
      )
        <**> helper
    )
    (fullDesc <> header "Lime 0.1.0.0 (c) Froxwin")

instance Peekable Scene where
  safepeek :: LuaError e => Peeker e Scene
  safepeek = peekViaJSON

getExt :: String -> String
getExt =
  dropWhile (/= '.') . reverse . takeWhile (`notElem` ['/', '\\']) . reverse

main :: IO ()
main = do
  (Lime i o p t m) <- execParser lime
  case getExt i of
    ".yaml" ->
      decodeFileEither i
        >>= ignite o p t m
          . either (error . prettyPrintParseException) id
    ".lua" ->
      run
        ( openlibs
            >> dofile (Just i)
            >> (getglobal :: Name -> LuaE Exception Type) "scene"
            >> peek top
        )
        >>= ignite o p t m
    ".anim.lua" -> do
      mn <- newManager
      run
        ( openlibs
            >> dofile (Just i)
            >> (getglobal :: Name -> LuaE Exception Type) "scenes"
            >> peek top
        )
        >>= mapM_
          ( \(j, s) ->
              forkManaged mn (ignite (o <> "/" <> show j <> ".png") p t m s)
          )
          . (\xs -> zip [1 .. length xs] xs)
      waitAll mn
    _ -> errorWithoutStackTrace "\ESC[1;31mInvalid scene format\ESC[0m"

data ThreadStatus
  = Running
  | Finished
  | Threw Exception
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
