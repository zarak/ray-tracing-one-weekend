module Ray where

import Vec3

data Ray = Ray
  { base :: Point,
    direction :: Vec3,
    time :: Double
  }
  deriving (Show)

at :: Ray -> Double -> Vec3
at ray t = ray.base.toVec3 + t *^ ray.direction
