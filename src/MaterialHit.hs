module MaterialHit where

import Color
import Ray
import Vec3

data Face = Front | Back

data HitRecord = HitRecord
  { p :: Point,
    normal :: Vec3,
    t :: Double,
    face :: Face, -- Front: ray is outside, Back: right is inside
    material :: Material
  }

newtype Material = Material
  { scatter :: Ray -> HitRecord -> Color -> Ray -> Bool
  }
