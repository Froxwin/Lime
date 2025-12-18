-- {-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE MagicHash #-}
-- {-# LANGUAGE UnliftedNewtypes #-}
-- module Main where
-- import Control.Monad (replicateM)
-- import Criterion.Main
-- import qualified Data.SIMD as S
-- import qualified Linear.V4 as L
-- import qualified Linear.Vector as L
-- import System.Random
-- chainF4 :: [S.F4] -> [Float] -> S.F4
-- chainF4 (v : vs) ss = go v vs ss
--  where
--   go !acc (x : xs) (s : sss) =
--     go ((acc S.^+^ x) S.^-^ (s S.*^ acc)) xs sss
--   go !acc _ _ = acc
-- chainLinear :: [L.V4 Float] -> [Float] -> L.V4 Float
-- chainLinear (v : vs) ss = go v vs ss
--  where
--   go !acc (x : xs) (s : sss) =
--     go ((acc L.^+^ x) L.^-^ (s L.*^ acc)) xs sss
--   go !acc _ _ = acc
-- -- ===== Random generation ===== --
-- randomF4 :: IO S.F4
-- randomF4 = S.V4 <$> randomIO <*> randomIO <*> randomIO <*> randomIO
-- randomL4 :: IO (L.V4 Float)
-- randomL4 = L.V4 <$> randomIO <*> randomIO <*> randomIO <*> randomIO
-- main :: IO ()
-- main = do
--   let sizes = [100, 1000, 10000]
--   randomSIMD <- mapM (\n -> replicateM n randomF4) sizes
--   randomLIN <- mapM (\n -> replicateM n randomL4) sizes
--   randomScals <- mapM (\n -> replicateM n randomIO) sizes
--   let [simd100, simd1000, simd10000] = randomSIMD
--       [lin100, lin1000, lin10000] = randomLIN
--       [s100, s1000, s10000] = randomScals
--   defaultMain
--     [ bgroup
--         "100 ops"
--         [ bench "SIMD F4" $ whnf (uncurry chainF4) (simd100, s100)
--         , bench "Linear V4" $ nf (uncurry chainLinear) (lin100, s100)
--         ]
--     , bgroup
--         "1000 ops"
--         [ bench "SIMD F4" $ whnf (uncurry chainF4) (simd1000, s1000)
--         , bench "Linear V4" $ nf (uncurry chainLinear) (lin1000, s1000)
--         ]
--     , bgroup
--         "10000 ops"
--         [ bench "SIMD F4" $ whnf (uncurry chainF4) (simd10000, s10000)
--         , bench "Linear V4" $ nf (uncurry chainLinear) (lin10000, s10000)
--         ]
--     ]
-- {-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE MagicHash #-}
-- {-# LANGUAGE UnliftedNewtypes #-}
-- module Main where
-- import Control.Monad (replicateM)
-- import Criterion.Main
-- import System.Random (randomIO)
-- import qualified Data.SIMD as S -- Your SIMD module
-- import qualified Linear.Matrix as L
-- import qualified Linear.V4 as L
-- import qualified Linear.Vector as L
-- -- chain (M * v) across lists
-- chainSIMD :: [S.M4] -> [S.F4] -> S.F4
-- chainSIMD (m : ms) (v : vs) = go (m S.!* v) ms vs
--  where
--   go !acc (m' : ms') (v' : vs') = go (m' S.!* acc S.^+^ v') ms' vs'
--   go !acc _ _ = acc
-- chainSIMD _ _ = error "empty input to chainSIMD"
-- chainLinear :: [L.M44 Float] -> [L.V4 Float] -> L.V4 Float
-- chainLinear (m : ms) (v : vs) = go (m L.!* v) ms vs
--  where
--   go !acc (m' : ms') (v' : vs') = go (m' L.!* acc L.^+^ v') ms' vs'
--   go !acc _ _ = acc
-- chainLinear _ _ = error "empty input to chainLinear"
-- -- ===== Random generation ===== --
-- randomF4 :: IO S.F4
-- randomF4 = S.V4 <$> randomIO <*> randomIO <*> randomIO <*> randomIO
-- randomM4 :: IO S.M4
-- randomM4 =
--   S.M44
--     <$> randomF4
--     <*> randomF4
--     <*> randomF4
--     <*> randomF4
-- randomL4 :: IO (L.V4 Float)
-- randomL4 = L.V4 <$> randomIO <*> randomIO <*> randomIO <*> randomIO
-- randomM44 :: IO (L.M44 Float)
-- randomM44 = do
--   a <- randomL4
--   b <- randomL4
--   c <- randomL4
--   d <- randomL4
--   pure (L.V4 a b c d)
-- main :: IO ()
-- main = do
--   let sizes = [100, 1000, 10000]
--   simdMatrices <- mapM (\n -> replicateM n randomM4) sizes
--   simdVectors <- mapM (\n -> replicateM n randomF4) sizes
--   linMatrices <- mapM (\n -> replicateM n randomM44) sizes
--   linVectors <- mapM (\n -> replicateM n randomL4) sizes
--   let [m100, m1000, m10000] = simdMatrices
--       [v100, v1000, v10000] = simdVectors
--       [lm100, lm1000, lm10000] = linMatrices
--       [lv100, lv1000, lv10000] = linVectors
--   defaultMain
--     [ bgroup
--         "100 ops"
--         [ bench "SIMD M4 * F4" $ whnf (uncurry chainSIMD) (m100, v100)
--         , bench "Linear M44 * V4" $ nf (uncurry chainLinear) (lm100, lv100)
--         ]
--     , bgroup
--         "1000 ops"
--         [ bench "SIMD M4 * F4" $ whnf (uncurry chainSIMD) (m1000, v1000)
--         , bench "Linear M44 * V4" $ nf (uncurry chainLinear) (lm1000, lv1000)
--         ]
--     , bgroup
--         "10000 ops"
--         [ bench "SIMD M4 * F4" $ whnf (uncurry chainSIMD) (m10000, v10000)
--         , bench "Linear M44 * V4" $ nf (uncurry chainLinear) (lm10000, lv10000)
--         ]
--     ]
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QualifiedDo #-}

import Control.Monad (replicateM)
import Criterion.Main
import Data.SIMD qualified as S
import Linear.Matrix qualified as L
import Linear.V4 qualified as L
import Linear.Vector qualified as L
import System.Random

-- Uniform floats in [-1, 1] to avoid overflow
uniformF :: IO Float
uniformF = fst . uniformR (0.1 :: Float, 0.9 :: Float) <$> getStdGen

-- Random SIMD vector and matrix (bounded)
randomF4 :: IO S.V4
randomF4 = S.V4 <$> uniformF <*> uniformF <*> uniformF <*> uniformF

randomM4 :: IO S.M44
randomM4 =
  S.M44 <$> randomF4 <*> randomF4 <*> randomF4 <*> randomF4

-- Random Linear equivalents (also bounded)
randomL4 :: IO (L.V4 Float)
randomL4 = L.V4 <$> uniformF <*> uniformF <*> uniformF <*> uniformF

randomL4x4 :: IO (L.M44 Float)
randomL4x4 =
  L.V4 <$> randomL4 <*> randomL4 <*> randomL4 <*> randomL4

main :: IO ()
main = do
  let sizes = [1, 2, 3]

  randomSIMDMats <- mapM (\n -> replicateM n randomM4) sizes :: IO [[S.M44]]
  randomLINMats <- mapM (\n -> replicateM n randomL4x4) sizes

  let [simd100, simd1000, simd10000] = randomSIMDMats
      [lin100, lin1000, lin10000] = randomLINMats
  print $ foldl1 (S.!*!) simd10000
  print $ foldl1 (L.!*!) lin10000
  defaultMain
    [ bgroup
        "100 ops"
        [ bench "SIMD" $ whnf (foldl1 (S.!*!)) simd100
        , bench "Linear" $ whnf (foldl1 (L.!*!)) lin100
        ]
    , bgroup
        "1000 ops"
        [ bench "SIMD" $ whnf (foldl1 (S.!*!)) simd1000
        , bench "Linear" $ whnf (foldl1 (L.!*!)) lin1000
        ]
    , bgroup
        "10000 ops"
        [ bench "SIMD" $ whnf (foldl1 (S.!*!)) simd10000
        , bench "Linear" $ whnf (foldl1 (L.!*!)) lin10000
        ]
    ]
