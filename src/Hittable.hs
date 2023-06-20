module Hittable where

import Ray
import Vec3

type TMin = Double

type TMax = Double

data HitRecord = HitRecord
  { p :: Point,
    normal :: Vec3,
    t :: Double
  }

class Hittable a where
  hit :: a -> Ray -> TMin -> TMax -> HitRecord -> Bool
