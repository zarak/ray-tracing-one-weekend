module MaterialHit where

import Color
import Ray
import Vec3

data Face = Front | Back deriving (Eq, Show, Enum)

data HitRecord = HitRecord
  { p :: Point,
    normal :: Vec3,
    t :: Double,
    face :: Face, -- Front: ray is outside, Back: right is inside
    material :: Material
  }

data Scattered = Scattered
  { ray :: Ray,
    attenuation :: Color
  }

newtype Material = Material
  { scatter :: Ray -> HitRecord -> Maybe Scattered
  }
