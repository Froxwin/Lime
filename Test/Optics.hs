{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Optics where

import Linear.Epsilon        (Epsilon (nearZero))
import Linear.Metric         (Metric (dot, norm), normalize)
import Linear.V3             (V3 (..))
import Linear.Vector         (Additive ((^-^)), negated)
import Test.Tasty            (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary (arbitrary), Gen, Testable (property),
                              testProperties, testProperty, (==>))

import Linear.Optics         (reflect, refract)

tests :: TestTree
tests = testGroup "Optics Tests" [propsReflection]

instance Arbitrary (V3 Double) where
  arbitrary :: Gen (V3 Double)
  arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary

propsReflection :: TestTree
propsReflection =
  testGroup
    "Reflection Tests"
    [ testProperty
        "Law of reflection"
        ( \v n ->
            nearZero
              ( (normalize (reflect v n) `dot` normalize n)
                  + (normalize (v :: V3 Double) `dot` normalize n)
              )
        )
    , testProperty
        "Reflection preserves length"
        (\v n -> nearZero (norm v - norm (reflect (v :: V3 Double) n)))
    ]

propsRefraction :: TestTree
propsRefraction =
  testProperties
    "Refraction Tests"
    [
      ( "Law of refraction"
      , property $ \v n mu ->
          let cosinei = (normalize (negated v :: V3 Double) `dot` normalize n)
              sinei = sin $ acos cosinei
              cosiner = normalize (negated $ refract v n mu) `dot` normalize n
              siner = sin $ acos cosiner
           in (mu :: Double)
                /= 0
                && norm v
                  /= 0
                && norm n
                  /= 0
                && v
                  `dot` n
                  <= 0
                ==> nearZero ((sinei / siner) - (1 / mu))
      )
    ,
      ( "Invariant when ior = 1"
      , property $
          \v n -> v `dot` n <= 0 ==> nearZero $ norm (v ^-^ refract v n 1)
      )
    ,
      ( "Refraction preserves length"
      , property $ \v n mu ->
          let theta = acos ((normalize v) `dot` (normalize n))
              crit = asin (1 / mu)
           in theta
                <= crit
                && v
                  `dot` n
                  <= 0
                ==> nearZero
                $ (norm v)
                  - (norm (refract v n mu))
      )
    ]
