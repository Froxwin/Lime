{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE PackageImports #-}

module Primitives where

import Codec.Picture

-- import Codec.Wavefront          hiding (Triangle)
import Data.ByteString.Internal
import Data.Maybe
import Data.Yaml
import "linear" Linear.Metric
import Test.Tasty
import Test.Tasty.HUnit
  ( assertBool
  , assertEqual
  , assertFailure
  , testCase
  , (@?)
  )
import Test.Tasty.QuickCheck

import Data.Color
import Data.Ray
import Lime.Camera
import Lime.Engine
import Lime.Internal.Hit hiding (texture)
import Lime.Internal.Utils
import Lime.Materials hiding (material)
import Lime.Primitives
import Lime.Textures hiding (texture)
import Linear (Epsilon (nearZero))
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
          primitive $
            Sphere (Lambertian $ SolidColor $ pure 1) []
   in testGroup
        "Sphere Primitive Tests"
        [ testGroup
            "Sphere Intersection Tests"
            [ testCase "Ray sphere intersection at tangent" $
                maybe
                  (assertFailure "Ray did not intersect sphere")
                  (assertEqual "Ray incorrectly hit sphere" (V3 0 1 0) . (.point) . fst)
                  (sphere (Ray (V3 0 1 (-5)) (V3 0 0 1)) 0 (1 / 0))
            , testCase "Ray missing sphere" $
                isNothing (sphere (Ray (V3 0 2 (-5)) (V3 0 0 1)) 0 (1 / 0))
                  @? "Ray incorrectly hit sphere"
            , testCase "Ray from inside sphere" $
                maybe
                  (assertFailure "Ray did not intersect sphere")
                  (assertEqual "Ray incorrectly hit sphere" (V3 0 0 1) . (.point) . fst)
                  (sphere (Ray (V3 0 0 0) (V3 0 0 1)) 0 (1 / 0))
            , testCase "Ray hits sphere directly at center" $
                let ray = Ray (V3 0 0 (-2)) (V3 0 0 1)
                    expected = V3 0 0 (-1)
                 in maybe
                      (assertFailure "Expected hit at sphere center")
                      (assertEqual "Hit point mismatch" expected . (.point) . fst)
                      (sphere ray 0 (1 / 0))
            , testCase "Ray hits off-center, verify hit point" $
                let ray = Ray (V3 1 0 (-5)) (V3 0 0 1)
                    expected = V3 1 0 (-1)
                 in maybe
                      (assertFailure "Expected off-center hit")
                      ( assertBool "Hit point mismatch" . nearZero . subtract expected . (.point) . fst
                      )
                      (sphere ray 0 (1 / 0))
            , testCase "Ray from inside sphere points outward" $
                let ray = Ray (V3 0 0 0) (V3 0 0 1)
                 in maybe
                      (assertFailure "Expected outward hit from inside")
                      (assertBool "t should be positive" . (> 0) . (.param) . fst)
                      (sphere ray 0 (1 / 0))
            , testCase "Ray in wrong direction misses" $
                let ray = Ray (V3 0 0 (-2)) (V3 1 0 0)
                 in isNothing (sphere ray 0 (1 / 0))
                      @? "Ray should miss sphere"
            , testCase "Ray hits but beyond t_max" $
                let ray = Ray (V3 0 0 (-5)) (V3 0 0 1)
                 in isNothing (sphere ray 0 1)
                      @? "Hit should be filtered by t_max"
            , testCase "Ray hits but before t_min" $
                let ray = Ray (V3 0 0 0) (V3 0 0 1)
                 in isNothing (sphere ray 1 2)
                      @? "Hit should be filtered by t_min"
            ]
        , testGroup
            "Sphere Normal Tests"
            [ testProperty "The normal is a normalized vector" $ \r ->
                norm r
                  /= 0
                    ==> nearZero
                      (norm (fst (fromJust $ sphere (Ray (pure 0) r) 0 (1 / 0))).normal - 1)
            , testProperty "Normal points away from center" $ \r ->
                let o = V3 0 0 0
                    ray = Ray o (normalize r)
                 in norm r
                      /= 0
                        ==> case sphere ray 0 (1 / 0) of
                          Nothing -> property True
                          Just (hit, _) ->
                            let expected = normalize (hit.point - o)
                             in property $ nearZero (norm (hit.normal - expected))
            ]
        ]
