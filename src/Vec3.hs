{-# LANGUAGE DuplicateRecordFields #-}

module Vec3 where

import Prelude hiding (length)

newtype Point = Point
  { toVec3 :: Vec3
  }
  deriving (Show)

newtype Color = Color
  { toVec3 :: Vec3
  }
  deriving (Show)

data Vec3 = Vec3
  { x :: Double,
    y :: Double,
    z :: Double
  }
  deriving (Show, Eq)

zeros :: Vec3
zeros = Vec3 0 0 0

instance Num Vec3 where
  v + w = Vec3 (v.x + w.x) (v.y + w.y) (v.z + w.z)
  v - w = Vec3 (v.x - w.x) (v.y - w.y) (v.z - w.z)
  v * w = Vec3 (v.x * w.x) (v.y * w.y) (v.z * w.z)

  -- Note that `abs` and `signum` must satisfy `abs x * signum x == x`, so they
  -- cannot be defined component-wise.
  abs = id
  signum _ = 1
  fromInteger n = let r = fromInteger n in Vec3 r r r

length :: Vec3 -> Double
length = sqrt . lengthSquared

lengthSquared :: Vec3 -> Double
lengthSquared v = v.x * v.x + v.y * v.y + v.z * v.z

scale :: Double -> Vec3 -> Vec3
scale t (Vec3 x y z) = Vec3 (t * x) (t * y) (t * z)

(*^) :: Double -> Vec3 -> Vec3
t *^ v = scale t v

infixr 7 *^

(^*) :: Vec3 -> Double -> Vec3
(^*) = flip scale

infixl 7 ^*

scaleDiv :: Vec3 -> Double -> Vec3
scaleDiv (Vec3 x y z) s = Vec3 (x / s) (y / s) (z / s)

(^/) :: Vec3 -> Double -> Vec3
v ^/ t = scaleDiv v t

infixl 7 ^/

unitVector :: Vec3 -> Vec3
unitVector v = v ^/ length v

dot :: Vec3 -> Vec3 -> Double
dot (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)

scaleVec3 :: Double -> Vec3 -> Vec3
scaleVec3 s (Vec3 x y z) = Vec3 (s * x) (s * y) (s * z)
