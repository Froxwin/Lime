{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Vec
import Linear qualified as L
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Linear.Transform

instance (Vector n, Arbitrary (Repr n)) => Arbitrary (Vec n) where
  arbitrary = pack <$> arbitrary

instance Arbitrary M44 where
  arbitrary = M44 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (L.V3 a) where
  arbitrary = L.V3 <$> arbitrary <*> arbitrary <*> arbitrary


instance Arbitrary a => Arbitrary (L.V4 a) where
  arbitrary = L.V4 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "SIMD Tests"
    [ vectorTests
    , crossTests
    , matrixTests
    , transformTests
    ]

transformTests = testGroup "Transform"
  [ testProperty "scale" $ \(v :: V3) -> norm (transform Data.Vec.point (mkTransform (Scale (L.V3 2 2 2))) v) === 2 * norm v

  ]

vectorTests :: TestTree
vectorTests =
  testGroup
    "Vectors"
    [ testProperty "pack . unpack = id (V2)" $
        \(v :: V2) -> (pack (unpack v)) === v
    , testProperty "pack . unpack = id (V3)" $
        \(v :: V3) -> (pack (unpack v)) === v
    , testProperty "pack . unpack = id (V4)" $
        \(v :: V4) -> (pack (unpack v)) === v
    , testProperty "v + 0 = v" $
        \(v :: V3) -> fromscalar 0 ^+^ v === v
    , testProperty "v - v = 0" $
        \(v :: V3) -> v ^-^ v === V3 0 0 0
    , testProperty
        "Addition check"
        $ \a@(V3 ax ay az) b@(V3 bx by bz) -> a ^+^ b === (V3 (ax + bx) (ay + by) (az + bz))
    , testProperty
        "Multiplication check"
        $ \a@(V3 ax ay az) b@(V3 bx by bz) -> a ^*^ b === (V3 (ax * bx) (ay * by) (az * bz))
    , testProperty
        "Subtraction check"
        $ \a@(V3 ax ay az) b@(V3 bx by bz) -> a ^-^ b === (V3 (ax - bx) (ay - by) (az - bz))
    , testProperty
        "Division check"
        $ \a@(V3 ax ay az) b@(V3 bx by bz) ->
          product [ax, ay, az, bx, by, bz] /= 0 ==>
            a ^/^ b === (V3 (ax / bx) (ay / by) (az / bz))
    , testProperty "Dot" $ \(v1 :: L.V3 Float) (v2 :: L.V3 Float) -> ((tosimd3 v1 :: V3) `dot` (tosimd3 v2)) === (v1 `L.dot` v2)
    ]

crossTests :: TestTree
crossTests =
  testGroup
    "Cross Product"
    [ testProperty "cross matches Linear's cross" $
        \(v1 :: V3) (v2 :: V3) ->
          let (V3 x1 y1 z1) = v1
              (V3 x2 y2 z2) = v2
              L.V3 lx ly lz = L.cross (L.V3 x1 y1 z1) (L.V3 x2 y2 z2)
              (V3 cx cy cz) = cross v1 v2
           in (cx, cy, cz) === (lx, ly, lz)
    , testProperty "v x v = 0" $
        \(v :: V3) ->
          cross v v === V3 0 0 0
    ]

matrixTests :: TestTree
matrixTests =
  testGroup
    "Matrices"
    [ testProperty "transpose . transpose = id" $
        \(m :: M44) -> transpose (transpose m) === m
    , testCase "identity !* v = v" $
        let idm =
              M44
                (V4 1 0 0 0)
                (V4 0 1 0 0)
                (V4 0 0 1 0)
                (V4 0 0 0 1)
            v = V4 3 4 5 6
         in idm !* v @?= v
    , testProperty "Multiplication" $ \(a :: L.M44 Float) (b :: L.M44 Float) ->
        let M44
              (V4 i00 i01 i02 i03)
              (V4 i10 i11 i12 i13)
              (V4 i20 i21 i22 i23)
              (V4 i30 i31 i32 i33) = ((tosimd44 a) !*! (tosimd44 b)) !-! (tosimd44 (a L.!*! b))
         in all (<= 1e-3) [i00, i01, i02, i03, i10, i11, i12, i13, i20, i21, i22, i23, i30, i31, i32, i33]
    , testProperty "Inversion" $ \(a :: L.M44 Float) ->
        let M44
              (V4 i00 i01 i02 i03)
              (V4 i10 i11 i12 i13)
              (V4 i20 i21 i22 i23)
              (V4 i30 i31 i32 i33) = (inv44 (tosimd44 a)) !-! (tosimd44 (L.inv44 a))
         in L.det44 a /= 0 ==> all (<= 1e-2) [i00, i01, i02, i03, i10, i11, i12, i13, i20, i21, i22, i23, i30, i31, i32, i33]
    , testProperty "Transposition" $ \(a :: L.M44 Float) ->
        let M44
              (V4 i00 i01 i02 i03)
              (V4 i10 i11 i12 i13)
              (V4 i20 i21 i22 i23)
              (V4 i30 i31 i32 i33) = (transpose (tosimd44 a)) !-! (tosimd44 (L.transpose a))
          in all (<= 1e-3) [i00, i01, i02, i03, i10, i11, i12, i13, i20, i21, i22, i23, i30, i31, i32, i33]
    , testProperty "Vector Mul" $ \(a :: L.M44 Float) (v :: L.V4 Float) ->
        let (V4 i0 i1 i2 i3) = ((tosimd44 a) !* (tosimd4 v)) ^-^ (tosimd4 (a L.!* v))
          in all (<= 1e-3) [i0, i1, i2, i3]

    ]

----------------------------------------------------------------------
-- Helper for approximate float matrix equality
----------------------------------------------------------------------

approx :: Float -> Float -> Bool
approx a b = abs (a - b) < 1e-4

approxVec :: V4 -> V4 -> Bool
approxVec (V4 a b c d) (V4 e f g h) =
  and [approx a e, approx b f, approx c g, approx d h]

approxMat :: M44 -> M44 -> Property
approxMat (M44 r0 r1 r2 r3) (M44 s0 s1 s2 s3) =
  counterexample (show (M44 r0 r1 r2 r3) <> "\nvs\n" <> show (M44 s0 s1 s2 s3)) $
    conjoin
      [ property (approxVec r0 s0)
      , property (approxVec r1 s1)
      , property (approxVec r2 s2)
      , property (approxVec r3 s3)
      ]
