{-# LANGUAGE DuplicateRecordFields #-}

module Vec3 where

import Prelude hiding (length)

type StartPoint = Point

type EndPoint = Point

newtype Point = Point
  { toVec3 :: Vec3
  }
  deriving (Show)

data Vec3 = Vec3
  { x :: Double,
    y :: Double,
    z :: Double
  }
  deriving (Show, Eq)

point :: Double -> Double -> Double -> Point
point a b c = Point $ Vec3 a b c

zeros :: Vec3
zeros = Vec3 0 0 0

instance Num Vec3 where
  v + w = Vec3 (v.x + w.x) (v.y + w.y) (v.z + w.z)
  v - w = Vec3 (v.x - w.x) (v.y - w.y) (v.z - w.z)
  v * w = Vec3 (v.x * w.x) (v.y * w.y) (v.z * w.z)
  abs (Vec3 x y z) = Vec3 (abs x) (abs y) (abs z)
  signum (Vec3 x y z) = Vec3 (signum x) (signum y) (signum z)
  fromInteger n = let r = fromInteger n in Vec3 r r r

instance Fractional Vec3 where
  v / w = Vec3 (v.x / w.x) (v.y / w.y) (v.z / w.z)
  fromRational r = let s = fromRational r in Vec3 s s s

length :: Vec3 -> Double
length = sqrt . lengthSquared

lengthSquared :: Vec3 -> Double
lengthSquared v = v.x * v.x + v.y * v.y + v.z * v.z

scaleVec3 :: Double -> Vec3 -> Vec3
scaleVec3 t (Vec3 x y z) = Vec3 (t * x) (t * y) (t * z)

(*^) :: Double -> Vec3 -> Vec3
t *^ v = scaleVec3 t v

infixr 7 *^

(^*) :: Vec3 -> Double -> Vec3
(^*) = flip scaleVec3

infixl 7 ^*

scaleDiv :: Vec3 -> Double -> Vec3
scaleDiv (Vec3 x y z) s = Vec3 (x / s) (y / s) (z / s)

(^/) :: Vec3 -> Double -> Vec3
v ^/ t = scaleDiv v t

infixl 7 ^/

(|->) :: StartPoint -> EndPoint -> Vec3
p1 |-> p2 = p2.toVec3 - p1.toVec3

infix 6 |->

unitVector :: Vec3 -> Vec3
unitVector v = v ^/ length v

dot :: Vec3 -> Vec3 -> Double
dot (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)

-- Map a vector of unit length to one with components in the range 0 to 1.
unitToInterval :: Vec3 -> Vec3
unitToInterval v = (1 + v) ^/ 2
