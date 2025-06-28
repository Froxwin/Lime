{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Primitives where

import Codec.Picture
import Codec.Wavefront          hiding (Triangle)
import Data.ByteString.Internal
import Data.Maybe
import Data.Yaml
import Linear.Metric
import Test.Tasty
import Test.Tasty.HUnit         (assertBool, assertEqual, assertFailure,
                                 testCase, (@?))
import Test.Tasty.QuickCheck

import Data.Color
import Data.Ray
import Lime.Camera
import Lime.Engine
import Lime.Internal.Hit        hiding (texture)
import Lime.Internal.Utils
import Lime.Materials           hiding (material)
import Lime.Primitives
import Lime.Textures            hiding (texture)
import Linear                   (Epsilon (nearZero))
import Linear.V3

tests :: TestTree
tests = testGroup "Primitive Tests" [sphereTests]

instance Arbitrary Ray where
  arbitrary :: Gen Ray
  arbitrary = Ray <$> arbitrary <*> arbitrary

instance Arbitrary (V3 Double) where
  arbitrary :: Gen (V3 Double)
  arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary

sphereTests :: TestTree
sphereTests =
  let sphere =
        head $
          primitive mempty $
            Sphere (Lambertian $ SolidColor $ pure 1) []
   in testGroup
        "Sphere Primitive Tests"
        [ testGroup
            "Sphere Intersection Tests"
            [ testCase "Ray sphere intersection at tangent" $
                maybe
                  (assertFailure "Ray did not intersect sphere")
                  (assertEqual "Ray incorrectly hit sphere" (V3 0 1 0) . (.point))
                  (sphere (Ray (V3 0 1 (-5)) (V3 0 0 1)) 0 (1 / 0))
            , testCase "Ray missing sphere" $
                isNothing (sphere (Ray (V3 0 2 (-5)) (V3 0 0 1)) 0 (1 / 0))
                  @? "Ray incorrectly hit sphere"
            , testCase "Ray from inside sphere" $
                maybe
                  (assertFailure "Ray did not intersect sphere")
                  (assertEqual "Ray incorrectly hit sphere" (V3 0 0 1) . (.point))
                  (sphere (Ray (V3 0 0 0) (V3 0 0 1)) 0 (1 / 0))
            ]
        , testGroup
            "Sphere Normal Tests"
            [ testProperty "The normal is a normalized vector" $ \r ->
                norm r /= 0 ==>
                  nearZero
                    (norm (fromJust $ sphere (Ray (pure 0) r) 0 (1 / 0)).normal - 1)
            ]
        ]
