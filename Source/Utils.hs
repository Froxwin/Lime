{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}

module Utils where

import           Data.Aeson.Types (GFromJSON, Options (..), Parser, Value, Zero,
                                   camelTo2, defaultOptions, genericParseJSON)
import           Data.Map         (Map)
import qualified Data.Map         as M
import           Data.Vector      (Vector)
import qualified Data.Vector      as V
import           GHC.Generics     (Generic (Rep))

import           Color            (Color)

makeImage :: Int -> [a] -> Vector (Vector a)
makeImage m ys = V.fromList $ group m ys
 where
  group _ [] = []
  group n xs = V.fromList (take n xs) : group n (drop n xs)

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

getPix :: [Color Double] -> Int -> Int -> Int -> Color Double
getPix img w x y = img !! ((w * y) + x)

worldParse :: (Generic a, GFromJSON Zero (Rep a)) => Value -> Parser a
worldParse =
  genericParseJSON
    defaultOptions
      { fieldLabelModifier = camelTo2 '_'
      , constructorTagModifier = camelTo2 '-'
      , rejectUnknownFields = True
      }

prettyError :: [Char] -> a
prettyError err = errorWithoutStackTrace $ "\ESC[1;31m" ++ err ++ "\ESC[0m"
