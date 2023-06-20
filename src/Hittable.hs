module Hittable where

import Ray
import Vec3

data HitRecord = HitRecord
  { p :: Point,
    normal :: Vec3,
    t :: Double
  }

class Hittable where
  hit :: Ray -> Double -> Double -> HitRecord
