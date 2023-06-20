module Hittable where

import Vec3

data Hit = Hit
  { p :: Point,
    normal :: Vec3,
    t :: Double
  }
