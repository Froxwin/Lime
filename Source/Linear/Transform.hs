{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Linear.Transform
  ( -- * Transformations and utilities
    Transform (Rotate, Scale, Translate)
  , transform
  , invTransform
  , mkTransform
  , mkTransforms

    -- * Re-export of matrix utilities
  , (!*)
  , M44
  , inv44
  , identity
  , point
  , vector
  ) where


import Control.DeepSeq (NFData)
import Control.Lens ((^.))
import Data.Aeson.Types (FromJSON (parseJSON), Parser, Value)
import GHC.Generics (Generic)
import Lime.Internal.Utils (worldParse)
import Linear.Matrix
  ( M44
  , fromQuaternion
  , identity
  , inv44
  , m33_to_m44
  , (!*)
  , (!*!)
  )
import Linear.Quaternion as Quaternion (axisAngle)
import Linear.V3 (R3 (_xyz), V3 (..))
import Linear.V4 (V4 (V4), point, vector)

{- NOTE [About Linear Transforms]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  The order of transformations should be Scale |> Rotate |> Translate
  so that the order of matrix multiplication is T * R * S
-}
data Transform
  = Translate !(V3 Float)
  | Scale !(V3 Float)
  | Rotate
      { axis :: !(V3 Float)
      , angle :: !Float
      }
  | Arbitrary !(M44 Float)
  deriving (Show, Eq, Generic, Ord)

instance NFData Transform

instance FromJSON (V4 Float)
instance FromJSON (M44 Float)

instance FromJSON Transform where
  parseJSON :: Value -> Parser Transform
  parseJSON = worldParse

transform
  , invTransform
    :: (V3 Float -> V4 Float)
    -- ^ See 'vector' and 'point'
    -> M44 Float
    -> V3 Float
    -> V3 Float
transform f tf v = (tf !* f v) ^. _xyz
invTransform f tf v = (inv44 tf !* f v) ^. _xyz
{-# INLINE transform #-}
{-# INLINE invTransform #-}

mkTransform :: Transform -> M44 Float
mkTransform (Translate (V3 tx ty tz)) =
  V4 (V4 1 0 0 tx) (V4 0 1 0 ty) (V4 0 0 1 tz) (V4 0 0 0 1)
mkTransform (Scale (V3 tx ty tz)) =
  V4 (V4 tx 0 0 0) (V4 0 ty 0 0) (V4 0 0 tz 0) (V4 0 0 0 1)
mkTransform (Rotate {..}) = m33_to_m44 $ fromQuaternion $ axisAngle axis angle
mkTransform (Arbitrary m) = m
{-# INLINE mkTransform #-}

-- | Make a transformation matrix from a foldable container of transformations
mkTransforms :: Foldable t => t Transform -> M44 Float
mkTransforms = foldr ((!*!) . mkTransform) identity
{-# INLINE mkTransforms #-}

-- hmm ts True =  inv44 $ foldr ((!*!) . mkTransform) identity ts
-- hmm ts False = foldr ((!*!) . inv44 .  mkTransform) identity ts
