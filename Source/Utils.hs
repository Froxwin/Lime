{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}

module Utils where

import           Data.Aeson.Types (GFromJSON, Options (..), Parser, Value, Zero,
                                   camelTo2, defaultOptions, genericParseJSON)
import           Data.Map         (Map)
import qualified Data.Map         as M
import           GHC.Generics     (Generic (Rep))

import           Color            (Color)

group :: Int -> [a] -> [[a]]
group _ [] = []
group n xs = take n xs : group n (drop n xs)

load
  :: (t -> IO (Either String b))
  -> String
  -> Maybe [(FilePath, t)]
  -> IO (Map String b)
load f e = fmap M.fromList . maybe
  (return [])
  (mapM (\(k, v) -> (k, ) . either error id <$> f v) . checkDupes)
 where
  checkDupes []            = []
  checkDupes ((a, b) : xs) = if a `elem` map fst xs
    then errorWithoutStackTrace
      $ concat ["Invalid ", e, "s: ", e, " `", a, "` defined twice"]
    else (a, b) : checkDupes xs

getPix :: [Color Double] -> Int -> Int -> Int -> Color Double
getPix img w x y = img !! ((w * y) + x)

worldParse :: (Generic a, GFromJSON Zero (Rep a)) => Int -> Value -> Parser a
worldParse n = genericParseJSON defaultOptions
  { fieldLabelModifier     = camelTo2 '-' . drop n
  , constructorTagModifier = camelTo2 '-'
  , rejectUnknownFields    = True
  }
