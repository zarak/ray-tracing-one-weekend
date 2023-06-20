module Ray where

import Vec3

data Ray = Ray
  { origin :: Point,
    direction :: Vec3
  }
  deriving (Show)

at :: Ray -> Double -> Vec3
at ray t = ray.origin.toVec3 + t *^ ray.direction
