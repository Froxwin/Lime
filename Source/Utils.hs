{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Utils where

import           Data.Aeson.Types               ( GFromJSON
                                                , Options(fieldLabelModifier)
                                                , Parser
                                                , Value
                                                , Zero
                                                , defaultOptions
                                                , genericParseJSON
                                                )
import           Data.Char                      ( isUpper
                                                , toLower
                                                )
import           GHC.Generics                   ( Generic(Rep) )

splitR :: (Char -> Bool) -> String -> [String]
splitR _ [] = []
splitR p s =
  let go m s' = case break p s' of
        (b', []    ) -> [m : b']
        (b', x : xs) -> (m : b') : go x xs
  in  case break p s of
        (b , []   ) -> [b]
        ([], h : t) -> go h t
        (b , h : t) -> b : go h t

pascal2Kebab :: String -> String
pascal2Kebab =
  map toLower
    . concat
    . underscores
    . splitR isUpper
    . (\case
        []     -> []
        x : xs -> toLower x : xs
      )
 where
  underscores []      = []
  underscores (h : t) = h : map ('-' :) t

-- | Drops prefix and converts from pascal case to kebab case
--
-- __Example__
--
-- > "texSomeTextureField" -> "some-texture-field"
worldParse :: (Generic a, GFromJSON Zero (Rep a)) => Int -> Value -> Parser a
worldParse n =
  genericParseJSON defaultOptions { fieldLabelModifier = pascal2Kebab . drop n }
