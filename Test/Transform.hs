{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE PackageImports #-}

module Transform where

import Control.Applicative
import Data.Bifunctor as BiFunc
import Data.Ray
import Linear.Epsilon
import Linear.Matrix
import "linear" Linear.Metric
import Linear.Optics
import Linear.Transform
import Linear.V3
import Linear.Vector
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Debug.Trace

tests :: TestTree
tests =
  testGroup
    "Transformation Tests"
    [translationTests, scalingTests, rotationTests]

translationTests :: TestTree
translationTests =
  testGroup
    "Translation Tests"
    [ testCase "Translation test" $
        let v = V3 1 2 3 :: V3 Double
            m = mkTransform (Translate (V3 3 4 5))
            v' = transform point m v
         in assertEqual "Incorrect translation" (V3 4 6 8) v'
    ]

scalingTests :: TestTree
scalingTests =
  testGroup
    "Scaling Tests"
    [ testCase "Scale test" $
        let v = V3 1 2 3 :: V3 Double
            m = mkTransform (Scale (V3 2 3 4))
            v' = transform Linear.Transform.vector m v
         in assertEqual "Incorrect Scale" (V3 2 6 12) v'
    , testProperty "Invariant when scaling by 1 in all directions" $
        \v ->
          nearZero
            ( norm
                (transform Linear.Transform.vector (mkTransform (Scale (V3 1 1 1))) v ^-^ v)
            )
    ]

instance Arbitrary a => Arbitrary (V3 a) where
  arbitrary :: Gen (V3 a)
  arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary

rotationTests :: TestTree
rotationTests =
  testGroup
    "Rotation Tests"
    [ testCase "Rotation test" $
        let v = V3 1 0 1 :: V3 Double
            a = mkTransform $ Rotate (V3 1 0 0) (pi / 2)
            b = mkTransform $ Scale (V3 5 5 5)
            c = mkTransform $ Translate (V3 10 5 7)
         in assertBool "Incorrect transform" $
              nearZero $
                norm
                  (V3 15 0 7 ^-^ transform point (c !*! b !*! a) v)
    , testProperty "Invariant when angle = 2pi" $ \v axis ->
        nearZero
          ( norm
              (transform Linear.Transform.vector (mkTransform (Rotate axis (2 * pi))) v ^-^ v)
          )
    ]

instance Arbitrary Ray where
  arbitrary = Ray <$> arbitrary <*> arbitrary

instance Arbitrary Transform where
  arbitrary = oneof [arbtranslate, arbscale, arbrotate]
   where
    arbtranslate = Translate <$> arbitrary
    arbscale = Scale <$> arbitrary
    arbrotate = Rotate <$> arbitrary <*> arbitrary
