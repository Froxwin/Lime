{-# LANGUAGE NoFieldSelectors #-}

module Lime.Context where

import Codec.Picture
import Data.Map (Map)
import Data.Wavefront

data RenderCtx = RenderCtx
  { textures :: Map String (Image PixelRGBF)
  , models :: Map String Object
  }
