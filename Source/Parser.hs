{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Parser where

import           Color        (Color)
import           Data.Yaml    (FromJSON)
import           GHC.Generics (Generic)
import           Materials    (dielectric, lambertian, metal, diffuseLight)
import           Things
import           Vector       (Vector)
import Textures (solidColor, checkerTexture, imageTexture)
import Codec.Picture

data WorldTexture =
    SolidColor
      { color :: Color
      }
  | Checkered
      { scale  :: Double
      , color1 :: Color
      , color2 :: Color
      }
  | ImageTexture
      { path :: FilePath
      }
  deriving (Show, Generic, Eq)

instance FromJSON WorldTexture

data WorldMaterial =
    Lambertian
      { texture :: WorldTexture
      }
  | Metal
      { texture :: WorldTexture
      , fuzz  :: Double
      }
  | Dielectric
      { ior :: Double
      }
  | Emissive
      { lightColor :: Color
      }
  deriving (Show, Generic, Eq)

instance FromJSON WorldMaterial

data WorldObject =
    Sphere
      { center   :: Vector
      , radius   :: Double
      , material :: WorldMaterial
      }
  | Quad
      { corner :: Vector
      , quadU :: Vector
      , quadV :: Vector
      , material :: WorldMaterial
      }
  deriving (Show, Generic)

instance FromJSON WorldObject

parseWorldObject :: [(DynamicImage, FilePath)] -> WorldObject  -> Thing
parseWorldObject _ (Sphere c r (Lambertian (SolidColor q))) = sphere c r (lambertian (solidColor q))
parseWorldObject _ (Sphere c r (Lambertian (Checkered k c1 c2))) = sphere c r (lambertian (checkerTexture k c1 c2))
parseWorldObject wq (Sphere c r (Lambertian (ImageTexture path))) = sphere c r (lambertian (imageTexture $ fst $ head $ filter (\(_,a) -> a == path) wq))
parseWorldObject _ (Sphere c r (Metal (SolidColor q) f   )) = sphere c r (metal (solidColor q) f)
parseWorldObject _ (Sphere c r (Metal (Checkered k c1 c2) f)) = sphere c r (metal (checkerTexture k c1 c2) f)
parseWorldObject _ (Sphere c r (Emissive cqc)) = sphere c r (diffuseLight cqc)
parseWorldObject _ (Sphere c r (Dielectric i)) = sphere c r (dielectric i)

parseWorldObject _ (Quad qqc qu qv (Lambertian (SolidColor q))) = quad qqc qu qv (lambertian (solidColor q))
parseWorldObject _ (Quad qqc qu qv (Lambertian (Checkered k c1 c2))) = quad qqc qu qv (lambertian (checkerTexture k c1 c2))
parseWorldObject wq (Quad qqc qu qv (Lambertian (ImageTexture path))) = quad qqc qu qv (lambertian (imageTexture $ fst $ head $ filter (\(_,a) -> a == path) wq))
parseWorldObject _ (Quad qqc qu qv (Metal (SolidColor q) f   )) = quad qqc qu qv (metal (solidColor q) f)
parseWorldObject _ (Quad qqc qu qv (Metal (Checkered k c1 c2) f)) = quad qqc qu qv (metal (checkerTexture k c1 c2) f)
parseWorldObject _ (Quad qqc qu qv (Emissive cqc)) = quad qqc qu qv (diffuseLight cqc)

-- parseWorldObject (Sphere c r (Dielectric i)) = sphere c r (dielectric i)
