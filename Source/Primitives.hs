{-# OPTIONS_GHC -Wno-partial-fields #-}

module Primitives where

import           Bounds                  hiding ( a )
import           Data.Aeson.Types               ( FromJSON(parseJSON)
                                                , Parser
                                                , Value
                                                )
import           Data.Maybe                     ( fromJust
                                                , isJust
                                                , isNothing
                                                )
import           GHC.Generics                   ( Generic )
import           Materials                      ( Material
                                                , TMaterial(material)
                                                , WorldMaterial
                                                )
import           Ray                            ( Ray(Ray)
                                                , rayAt
                                                )
import           Textures                       ( TextureCoords )
import           Utils                          ( worldParse )
import           Vector                         ( Vector(Vector)
                                                , cross
                                                , dot
                                                , magnitude
                                                , normalize
                                                , vadd
                                                , vmul
                                                , vsub
                                                )

type Primitive
  =  Ray
  -> Double
  -> Double
  -> Maybe (Vector, Vector, Double, Material, TextureCoords)

class TWorldObject a where
    primitive :: a -> Primitive
    boundingBox :: a -> AABB

data WorldObject
    = Sphere
        { primCenter   :: Vector
        , primRadius   :: Double
        , primMaterial :: WorldMaterial
        }
    | Quad
        { primCorner   :: Vector
        , primQuadU    :: Vector
        , primQuadV    :: Vector
        , primMaterial :: WorldMaterial
        }
    deriving ( Show, Generic )

instance FromJSON WorldObject where
  parseJSON :: Value -> Parser WorldObject
  parseJSON = worldParse 4

instance TWorldObject WorldObject where
  primitive :: WorldObject -> Primitive
  -----------------------------------------------------------------------------
  -- Sphere Primitive Intersection
  -----------------------------------------------------------------------------
  primitive (Sphere c r m) ray@(Ray a d) tMin tMax
    | delta < 0 = Nothing
    | t' < t = if isWithin t'
      then Just (normal t', point t', t', material m, getUV $ normal t')
      else Nothing
    | otherwise = if isWithin t
      then Just (normal t, point t, t, material m, getUV $ normal t)
      else Nothing
   where
    delta =
      (d `dot` (a `vsub` c))
        ^ 2
        - magnitude d
        ^ 2
        * (magnitude (a `vsub` c) ^ 2 - r ^ 2)
    root pm = ((-(d `dot` (a `vsub` c))) `pm` sqrt delta) / magnitude d ^ 2
    t     = root (+)
    t'    = root (-)
    point = rayAt ray
    normal x = normalize (point x `vsub` c)
    isWithin x = x >= tMin && x <= tMax
    getUV (Vector px py pz) = (phi / (2 * pi), theta / pi)
     where
      theta = acos (-py)
      phi   = atan2 (-pz) px + pi
  -----------------------------------------------------------------------------
  -- Quadrilateral Primitive Intersection
  -----------------------------------------------------------------------------
  primitive (Quad q u v m) ray@(Ray a dir) tMin tMax
    | not $ isWithin t       = Nothing
    | isNothing someThingUhh = Nothing
    | otherwise              = Just
      (normal, rayAt ray t, t, material m, fromJust someThingUhh)
   where
    isWithin x = x >= tMin && x <= tMax
    n             = u `cross` v
    normal        = normalize n
    d             = normal `dot` q
    denom         = normal `dot` dir
    t             = (d - (normal `dot` a)) / denom
    w             = (1 / (n `dot` n)) `vmul` n
    intersection  = rayAt ray t
    planerHitpVec = intersection `vsub` q
    alpha         = w `dot` (planerHitpVec `cross` v)
    beta          = w `dot` (u `cross` planerHitpVec)
    someThingUhh  = isIn alpha beta
    isIn g1 g2 | g1 < 0 || 1 < g1 || g2 < 0 || 1 < g2 = Nothing
               | otherwise                            = Just (g1, g2)

  boundingBox :: WorldObject -> AABB
  -----------------------------------------------------------------------------
  -- Sphere Primitive Bounding Box
  -----------------------------------------------------------------------------
  boundingBox (Sphere c r _) = AABB (c `vsub` vecr) (c `vadd` vecr)
    where vecr = Vector r r r
  -----------------------------------------------------------------------------
  -- Quadrilateral Primitive Bounding Box
  -----------------------------------------------------------------------------
  boundingBox (Quad q u v _) = AABB q (q `vadd` u `vadd` v)

data BVHNode = BVHNode
  { ls    :: [Primitive]
  , bbox  :: AABB
  , left  :: Primitive
  , right :: Primitive
  }

instance TWorldObject BVHNode where
  primitive aabb ray tMin tMax
    | null $ primitive aabb ray tMin tMax = Nothing
    | hitLeft                             = Just lft
    | hitRight                            = Just rft
    | otherwise = error "How could this have conspired"
   where
    hitLeft = isJust $ left aabb ray tMin tMax
    lft@(_, _, t, _, _) = fromJust $ left aabb ray tMin tMax
    hitRight = isJust $ right aabb ray tMin (if hitLeft then t else tMax)
    rft = fromJust $ right aabb ray tMin (if hitLeft then t else tMax)

  boundingBox (BVHNode _ qw _ _) = qw
