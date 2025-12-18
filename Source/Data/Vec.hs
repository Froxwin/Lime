{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Vec where

import Control.DeepSeq
import Data.Data (Proxy (Proxy))
import GHC.Exts
import GHC.Prim
import GHC.TypeLits
import Linear.V3 qualified as L
import Linear.V4 qualified as L

class Vector (n :: Nat) where
  type Repr n
  pack :: Repr n -> Vec n
  unpack :: Vec n -> Repr n

data Vec (n :: Nat) = Vec FloatX4#

instance NFData (Vec n) where
  rnf v = v `seq` ()

instance KnownNat n => Eq (Vec n) where
  (==) (Vec x#) (Vec y#) =
    let !(# x0#, x1#, x2#, x3# #) = unpackFloatX4# x#
        !(# y0#, y1#, y2#, y3# #) = unpackFloatX4# y#
        n = fromIntegral (natVal (Proxy @n))
     in and $
          take
            n
            [ isTrue# (eqFloat# x0# y0#)
            , isTrue# (eqFloat# x1# y1#)
            , isTrue# (eqFloat# x2# y2#)
            , isTrue# (eqFloat# x3# y3#)
            ]

minVec (Vec v1#) (Vec v2#) = Vec $ minFloatX4# v1# v2#

maxVec (Vec v1#) (Vec v2#) = Vec $ maxFloatX4# v1# v2#

infixl 6 ^+^, ^-^
(^+^), (^-^), (^*^), (^/^) :: Vec n -> Vec n -> Vec n
(^+^) (Vec v1#) (Vec v2#) = Vec $ plusFloatX4# v1# v2#
(^-^) (Vec v1#) (Vec v2#) = Vec $ minusFloatX4# v1# v2#
(^*^) (Vec v1#) (Vec v2#) = Vec $ timesFloatX4# v1# v2#
(^/^) (Vec v1#) (Vec v2#) = Vec $ divideFloatX4# v1# v2#

infixl 7 *^, /^, ^*, ^/
(*^), (/^) :: Float -> Vec n -> Vec n
(*^) (F# s#) (Vec v#) = Vec $ timesFloatX4# v# (broadcastFloatX4# s#)
(/^) (F# s#) (Vec v#) = Vec $ divideFloatX4# v# (broadcastFloatX4# s#)

(^*), (^/) :: Vec n -> Float -> Vec n
(^*) (Vec v#) (F# s#) = Vec $ timesFloatX4# v# (broadcastFloatX4# s#)
(^/) (Vec v#) (F# s#) = Vec $ divideFloatX4# v# (broadcastFloatX4# s#)

dot :: Vec n -> Vec n -> Float
dot (Vec v1#) (Vec v2#) =
  let !(# x#, y#, z#, w# #) = unpackFloatX4# (timesFloatX4# v1# v2#)
   in F# $ plusFloat# (plusFloat# x# y#) (plusFloat# z# w#)

quadrance :: Vec n -> Float
quadrance v = dot v v

norm :: Vec n -> Float
norm v = sqrt (dot v v)

normalize :: Vec n -> Vec n
normalize v = norm v /^ v

negated :: Vec n -> Vec n
negated (Vec v#) = Vec $ negateFloatX4# v#

cross :: V3 -> V3 -> V3
cross (Vec v1#) (Vec v2#) =
  Vec $
    packFloatX4#
      (#
        minusFloat# (timesFloat# ay# bz#) (timesFloat# az# by#)
        , minusFloat# (timesFloat# az# bx#) (timesFloat# ax# bz#)
        , minusFloat# (timesFloat# ax# by#) (timesFloat# ay# bx#)
        , 0.0#
      #)
 where
  !(# ax#, ay#, az#, _ #) = unpackFloatX4# v1#
  !(# bx#, by#, bz#, _ #) = unpackFloatX4# v2#

fromscalar :: Float -> Vec n
fromscalar (F# s#) = Vec $ broadcastFloatX4# s#

tosimd3 (L.V3 x y z) = pack (x, y, z)
tosimd4 (L.V4 x y z w) = pack (x, y, z, w)

getX :: Vec n -> Float
getX (Vec v#) = let !(# x#, _, _, _ #) = unpackFloatX4# v# in F# x#

getY :: Vec n -> Float
getY (Vec v#) = let !(# _, y#, _, _ #) = unpackFloatX4# v# in F# y#

getZ :: 3 <= n => Vec n -> Float
getZ (Vec v#) = let !(# _, _, z#, _ #) = unpackFloatX4# v# in F# z#

getW :: Vec 4 -> Float
getW (Vec v#) = let !(# _, _, _, w# #) = unpackFloatX4# v# in F# w#

type V2 = Vec 2

pattern V2 :: Float -> Float -> Vec 2
pattern V2 x y <- (unpack -> (x, y))
  where
    V2 x y = pack (x, y)
{-# COMPLETE V2 #-}

instance Vector 2 where
  type Repr 2 = (Float, Float)
  pack (F# x#, F# y#) = Vec (packFloatX4# (# x#, y#, 0.0#, 0.0# #))
  unpack (Vec v#) = case unpackFloatX4# v# of
    (# x#, y#, _, _ #) -> (F# x#, F# y#)

instance Show (Vec 2) where
  show v = show $ unpack v

type V3 = Vec 3

pattern V3 :: Float -> Float -> Float -> Vec 3
pattern V3 x y z <- (unpack -> (x, y, z))
  where
    V3 x y z = pack (x, y, z)
{-# COMPLETE V3 #-}

instance Vector 3 where
  type Repr 3 = (Float, Float, Float)
  pack (F# x#, F# y#, F# z#) = Vec (packFloatX4# (# x#, y#, z#, 0.0# #))
  unpack (Vec v#) = case unpackFloatX4# v# of
    (# x#, y#, z#, _ #) -> (F# x#, F# y#, F# z#)

instance Show (Vec 3) where
  show v = show $ unpack v

type V4 = Vec 4

pattern V4 :: Float -> Float -> Float -> Float -> Vec 4
pattern V4 x y z w <- (unpack -> (x, y, z, w))
  where
    V4 x y z w = pack (x, y, z, w)
{-# COMPLETE V4 #-}

instance Vector 4 where
  type Repr 4 = (Float, Float, Float, Float)
  pack (F# x#, F# y#, F# z#, F# w#) = Vec (packFloatX4# (# x#, y#, z#, w# #))
  unpack (Vec v#) = case unpackFloatX4# v# of
    (# x#, y#, z#, w# #) -> (F# x#, F# y#, F# z#, F# w#)

instance Show (Vec 4) where
  show v = show $ unpack v

data Mat (n :: Nat) = Mat FloatX4# FloatX4# FloatX4# FloatX4#

eqFloatX4# x# y# =
  let !(# x0#, x1#, x2#, x3# #) = unpackFloatX4# x#
      !(# y0#, y1#, y2#, y3# #) = unpackFloatX4# y#
   in and
        [ isTrue# (eqFloat# x0# y0#)
        , isTrue# (eqFloat# x1# y1#)
        , isTrue# (eqFloat# x2# y2#)
        , isTrue# (eqFloat# x3# y3#)
        ]

instance KnownNat n => Eq (Mat n) where
  (==) (Mat x0# x1# x2# x3#) (Mat y0# y1# y2# y3#) =
    and
      [ eqFloatX4# x0# y0#
      , eqFloatX4# x1# y1#
      , eqFloatX4# x2# y2#
      , eqFloatX4# x3# y3#
      ]

type M44 = Mat 4

pattern M44 :: Vec 4 -> Vec 4 -> Vec 4 -> Vec 4 -> Mat 4
pattern M44 r0 r1 r2 r3 <-
  Mat (Vec -> r0) (Vec -> r1) (Vec -> r2) (Vec -> r3)
  where
    M44 (Vec r0#) (Vec r1#) (Vec r2#) (Vec r3#) =
      Mat r0# r1# r2# r3#
{-# COMPLETE M44 #-}

dotUnboxed v1# v2# =
  let !(# x#, y#, z#, w# #) = unpackFloatX4# (timesFloatX4# v1# v2#)
   in plusFloat# (plusFloat# x# y#) (plusFloat# z# w#)

infixl 7 !*, *!!
(!*) :: Mat 4 -> Vec 4 -> Vec 4
(!*) (Mat a0# a1# a2# a3#) (Vec v#) =
  Vec $
    packFloatX4#
      (#
        dotUnboxed a0# v#
        , dotUnboxed a1# v#
        , dotUnboxed a2# v#
        , dotUnboxed a3# v#
      #)

(*!!) :: Float -> Mat 4 -> Mat 4
(*!!) (F# s) (Mat a0 a1 a2 a3) =
  Mat
    (timesFloatX4# t a0)
    (timesFloatX4# t a1)
    (timesFloatX4# t a2)
    (timesFloatX4# t a3)
 where
  t = broadcastFloatX4# s

(!*#) (Mat a0# a1# a2# a3#) v# =
  packFloatX4#
    (#
      dotUnboxed a0# v#
      , dotUnboxed a1# v#
      , dotUnboxed a2# v#
      , dotUnboxed a3# v#
    #)

(!-!) :: Mat 4 -> Mat 4 -> Mat 4
(!-!) (Mat a0 a1 a2 a3) (Mat b0 b1 b2 b3) =
  Mat
    (minusFloatX4# a0 b0)
    (minusFloatX4# a1 b1)
    (minusFloatX4# a2 b2)
    (minusFloatX4# a3 b3)

(!*!) :: Mat 4 -> Mat 4 -> Mat 4
(!*!) a (Mat b0 b1 b2 b3) = transpose $ Mat (a !*# q0) (a !*# q1) (a !*# q2) (a !*# q3)
 where
  q0 = packFloatX4# (# b00#, b10#, b20#, b30# #)
  q1 = packFloatX4# (# b01#, b11#, b21#, b31# #)
  q2 = packFloatX4# (# b02#, b12#, b22#, b32# #)
  q3 = packFloatX4# (# b03#, b13#, b23#, b33# #)
  !(# b00#, b01#, b02#, b03# #) = unpackFloatX4# b0
  !(# b10#, b11#, b12#, b13# #) = unpackFloatX4# b1
  !(# b20#, b21#, b22#, b23# #) = unpackFloatX4# b2
  !(# b30#, b31#, b32#, b33# #) = unpackFloatX4# b3

instance Show (Mat 4) where
  show (Mat v0# v1# v2# v3#) = unwords $ map (show @(Vec 4)) [Vec v0#, Vec v1#, Vec v2#, Vec v3#]

vector, point :: V3 -> V4
vector (Vec v#) = Vec $ insertFloatX4# v# 0.0# 3#
point (Vec v#) = Vec $ insertFloatX4# v# 1.0# 3#
{-# INLINE vector #-}
{-# INLINE point #-}

transform :: (V3 -> V4) -> M44 -> V3 -> V3
transform f tf v = let !(Vec v#) = tf !* f v in Vec v#
{-# INLINE transform #-}

transpose (Mat r0# r1# r2# r3#) = Mat c0# c1# c2# c3#
 where
  !c0# = packFloatX4# (# i00, i10, i20, i30 #)
  !c1# = packFloatX4# (# i01, i11, i21, i31 #)
  !c2# = packFloatX4# (# i02, i12, i22, i32 #)
  !c3# = packFloatX4# (# i03, i13, i23, i33 #)
  !(# i00, i01, i02, i03 #) = unpackFloatX4# r0#
  !(# i10, i11, i12, i13 #) = unpackFloatX4# r1#
  !(# i20, i21, i22, i23 #) = unpackFloatX4# r2#
  !(# i30, i31, i32, i33 #) = unpackFloatX4# r3#

inv44 (Mat r0# r1# r2# r3#) =
  F# invDet
    *!! Mat
      ( packFloatX4#
          (#
            (i11 `timesFloat#` c5)
              `minusFloat#` (i12 `timesFloat#` c4)
              `plusFloat#` (i13 `timesFloat#` c3)
            , ((negateFloat# i01) `timesFloat#` c5)
                `plusFloat#` (i02 `timesFloat#` c4)
                `minusFloat#` (i03 `timesFloat#` c3)
            , (i31 `timesFloat#` s5)
                `minusFloat#` (i32 `timesFloat#` s4)
                `plusFloat#` (i33 `timesFloat#` s3)
            , ((negateFloat# i21) `timesFloat#` s5)
                `plusFloat#` (i22 `timesFloat#` s4)
                `minusFloat#` (i23 `timesFloat#` s3)
          #)
      )
      ( packFloatX4#
          (#
            ((negateFloat# i10) `timesFloat#` c5)
              `plusFloat#` (i12 `timesFloat#` c2)
              `minusFloat#` (i13 `timesFloat#` c1)
            , (i00 `timesFloat#` c5)
                `minusFloat#` (i02 `timesFloat#` c2)
                `plusFloat#` (i03 `timesFloat#` c1)
            , ((negateFloat# i30) `timesFloat#` s5)
                `plusFloat#` (i32 `timesFloat#` s2)
                `minusFloat#` (i33 `timesFloat#` s1)
            , (i20 `timesFloat#` s5)
                `minusFloat#` (i22 `timesFloat#` s2)
                `plusFloat#` (i23 `timesFloat#` s1)
          #)
      )
      ( packFloatX4#
          (#
            (i10 `timesFloat#` c4)
              `minusFloat#` (i11 `timesFloat#` c2)
              `plusFloat#` (i13 `timesFloat#` c0)
            , ((negateFloat# i00) `timesFloat#` c4)
                `plusFloat#` (i01 `timesFloat#` c2)
                `minusFloat#` (i03 `timesFloat#` c0)
            , (i30 `timesFloat#` s4)
                `minusFloat#` (i31 `timesFloat#` s2)
                `plusFloat#` (i33 `timesFloat#` s0)
            , ((negateFloat# i20) `timesFloat#` s4)
                `plusFloat#` (i21 `timesFloat#` s2)
                `minusFloat#` (i23 `timesFloat#` s0)
          #)
      )
      ( packFloatX4#
          (#
            ( (negateFloat# i10)
                `timesFloat#` c3
            )
              `plusFloat#` ( i11
                               `timesFloat#` c1
                           )
              `minusFloat#` ( i12
                                `timesFloat#` c0
                            )
            , ( i00
                  `timesFloat#` c3
              )
                `minusFloat#` ( i01
                                  `timesFloat#` c1
                              )
                `plusFloat#` ( i02
                                 `timesFloat#` c0
                             )
            , ( (negateFloat# i30)
                  `timesFloat#` s3
              )
                `plusFloat#` ( i31
                                 `timesFloat#` s1
                             )
                `minusFloat#` ( i32
                                  `timesFloat#` s0
                              )
            , ( i20
                  `timesFloat#` s3
              )
                `minusFloat#` ( i21
                                  `timesFloat#` s1
                              )
                `plusFloat#` ( i22
                                 `timesFloat#` s0
                             )
          #)
      )
 where
  invDet = divideFloat# 1.0# det
  det =
    (s0 `timesFloat#` c5)
      `minusFloat#` (s1 `timesFloat#` c4)
      `plusFloat#` (s2 `timesFloat#` c3)
      `plusFloat#` (s3 `timesFloat#` c2)
      `minusFloat#` (s4 `timesFloat#` c1)
      `plusFloat#` (s5 `timesFloat#` c0)
  s0 = minusFloat# (timesFloat# i00 i11) (timesFloat# i10 i01)
  s1 = minusFloat# (timesFloat# i00 i12) (timesFloat# i10 i02)
  s2 = minusFloat# (timesFloat# i00 i13) (timesFloat# i10 i03)
  s3 = minusFloat# (timesFloat# i01 i12) (timesFloat# i11 i02)
  s4 = minusFloat# (timesFloat# i01 i13) (timesFloat# i11 i03)
  s5 = minusFloat# (timesFloat# i02 i13) (timesFloat# i12 i03)
  c5 = minusFloat# (timesFloat# i22 i33) (timesFloat# i32 i23)
  c4 = minusFloat# (timesFloat# i21 i33) (timesFloat# i31 i23)
  c3 = minusFloat# (timesFloat# i21 i32) (timesFloat# i31 i22)
  c2 = minusFloat# (timesFloat# i20 i33) (timesFloat# i30 i23)
  c1 = minusFloat# (timesFloat# i20 i32) (timesFloat# i30 i22)
  c0 = minusFloat# (timesFloat# i20 i31) (timesFloat# i30 i21)
  !(# i00, i01, i02, i03 #) = unpackFloatX4# r0#
  !(# i10, i11, i12, i13 #) = unpackFloatX4# r1#
  !(# i20, i21, i22, i23 #) = unpackFloatX4# r2#
  !(# i30, i31, i32, i33 #) = unpackFloatX4# r3#

identity = M44 (V4 1 0 0 0) (V4 0 1 0 0) (V4 0 0 1 0) (V4 0 0 0 1)

tosimd44
  ( L.V4
      (L.V4 (F# i00) (F# i01) (F# i02) (F# i03))
      (L.V4 (F# i10) (F# i11) (F# i12) (F# i13))
      (L.V4 (F# i20) (F# i21) (F# i22) (F# i23))
      (L.V4 (F# i30) (F# i31) (F# i32) (F# i33))
    ) = Mat c0# c1# c2# c3#
   where
    !c0# = packFloatX4# (# i00, i01, i02, i03 #)
    !c1# = packFloatX4# (# i10, i11, i12, i13 #)
    !c2# = packFloatX4# (# i20, i21, i22, i23 #)
    !c3# = packFloatX4# (# i30, i31, i32, i33 #)

-- data Mat (n :: Nat) = Mat ByteArray#

-- pattern M44 :: Vec 4 -> Vec 4 -> Vec 4 -> Vec 4 -> Mat 4
-- pattern M44 r0 r1 r2 r3 <- (unpackM44 -> (r0, r1, r2, r3))
--   where
--     M44 r0 r1 r2 r3 = packM44 r0 r1 r2 r3

-- unpackM44 :: Mat 4 -> (Vec 4, Vec 4, Vec 4, Vec 4)
-- unpackM44 (Mat arr#) =
--   let r0# = indexFloatX4Array# arr# 0#
--       r1# = indexFloatX4Array# arr# 1#
--       r2# = indexFloatX4Array# arr# 2#
--       r3# = indexFloatX4Array# arr# 3#
--    in (Vec r0#, Vec r1#, Vec r2#, Vec r3#)

-- packM44 :: Vec 4 -> Vec 4 -> Vec 4 -> Vec 4 -> Mat 4
-- packM44 (Vec a#) (Vec b#) (Vec c#) (Vec F#) = runST $ newM44 a# b# c# F#

-- newM44 :: FloatX4# -> FloatX4# -> FloatX4# -> FloatX4# -> ST s (Mat 4)
-- newM44 r0# r1# r2# r3# =
--   primitive $ \s0# -> case newAlignedPinnedByteArray# 64# 16# s0# of
--     (# s1#, mba# #) ->
--       let s2# = writeFloatX4Array# mba# 0# r0# s1#
--           s3# = writeFloatX4Array# mba# 1# r1# s2#
--           s4# = writeFloatX4Array# mba# 2# r2# s3#
--           s5# = writeFloatX4Array# mba# 3# r3# s4#
--        in case unsafeFreezeByteArray# mba# s5# of
--             (# s6#, arr# #) -> (# s6#, Mat arr# #)

-- (!*!) (Mat m1#) (Mat m2#) = undefined
--   where
--     ar0 = indexFloatX4Array# m1# 0#
--     ar1 = indexFloatX4Array# m1# 1#
--     ar2 = indexFloatX4Array# m1# 2#
--     ar3 = indexFloatX4Array# m1# 3#

-- pattern M44 :: Vec 4 -> Vec 4 -> Vec 4 -> Vec 4 -> Mat 4
-- pattern M44 r0 r1 r2 r3 <-
--   Mat (Vec -> r0) (Vec -> r1) (Vec -> r2) (Vec -> r3)
--   where
--     M44 (Vec r0#) (Vec r1#) (Vec r2#) (Vec r3#) =
--       Mat r0# r1# r2# r3#

-- data F4 = F4 FloatX4#

-- instance Show F4 where
--   show (F4 v#) =
--     case unpackFloatX4# v# of
--       (# x#, y#, z#, w# #) ->
--         "F4 "
--           ++ unwords
--             (map show [F# x#, F# y#, F# z#, F# w#])

-- packF4 :: Float -> Float -> Float -> Float -> F4
-- packF4 (F# a#) (F# b#) (F# c#) (F# F#) = F4 (packFloatX4# (# a#, b#, c#, F# #))

-- unpackF4 :: F4 -> (Float, Float, Float, Float)
-- unpackF4 (F4 v#) = case unpackFloatX4# v# of
--   (# a#, b#, c#, F# #) -> (F# a#, F# b#, F# c#, F# F#)

-- pattern V4 :: Float -> Float -> Float -> Float -> F4
-- pattern V4 a b c d <- (unpackF4 -> (a, b, c, d))
--   where
--     V4 a b c d = packF4 a b c d

-- (^+^) :: F4 -> F4 -> F4
-- (^+^) (F4 x#) (F4 y#) = F4 (plusFloatX4# x# y#)

-- (^-^) :: F4 -> F4 -> F4
-- (^-^) (F4 x#) (F4 y#) = F4 (minusFloatX4# x# y#)

-- (*^) :: Float -> F4 -> F4
-- (*^) (F# s#) (F4 v#) = F4 (timesFloatX4# (broadcastFloatX4# s#) v#)

-- data M4 = M4 FloatX4# FloatX4# FloatX4# FloatX4#

-- packM4 :: F4 -> F4 -> F4 -> F4 -> M4
-- packM4 (F4 a#) (F4 b#) (F4 c#) (F4 F#) =
--   M4 a# b# c# F#

-- unpackM4 :: M4 -> (F4, F4, F4, F4)
-- unpackM4 (M4 a# b# c# F#) =
--   (F4 a#, F4 b#, F4 c#, F4 F#)

-- pattern M44 :: F4 -> F4 -> F4 -> F4 -> M4
-- pattern M44 r1 r2 r3 r4 <- (unpackM4 -> (r1, r2, r3, r4))
--   where
--     M44 r1 r2 r3 r4 = packM4 r1 r2 r3 r4

-- sumF4 :: FloatX4# -> Float#
-- sumF4 v# =
--   case unpackFloatX4# v# of
--     (# a#, b#, c#, F# #) -> plusFloat# (plusFloat# a# b#) (plusFloat# c# F#)

-- (!*) :: M4 -> F4 -> F4
-- (!*) (M4 m1# m2# m3# m4#) (F4 v#) =
--   F4
--     ( packFloatX4#
--         (#
--           sumF4 (timesFloatX4# v# m1#)
--           , sumF4 (timesFloatX4# v# m2#)
--           , sumF4 (timesFloatX4# v# m3#)
--           , sumF4 (timesFloatX4# v# m4#)
--         #)
--     )

-- transposeM4 :: M4 -> M4
-- transposeM4
--   ( M44
--       (V4 a b c d)
--       (V4 e f g h)
--       (V4 i j k l)
--       (V4 m n o p)
--     ) =
--     M44
--       (V4 a e i m)
--       (V4 b f j n)
--       (V4 c g k o)
--       (V4 d h l p)

-- (!*!) :: M4 -> M4 -> M4
-- (!*!) mA mB =
--   let M44 c1 c2 c3 c4 = transposeM4 mB
--    in M44
--         (mA !* c1)
--         (mA !* c2)
--         (mA !* c3)
--         (mA !* c4)
