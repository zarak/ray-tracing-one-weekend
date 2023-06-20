module Hittable where

import Ray
import Vec3

type TMin = Double

type TMax = Double

type Root = Double

data Face = Front | Back

data HitRecord = HitRecord
  { p :: Point,
    normal :: Vec3,
    t :: Double,
    face :: Face -- Front: ray is outside, Back: right is inside
  }

class Hittable a where
  hit :: a -> Ray -> (TMin, TMax) -> Maybe HitRecord

setFaceNormal :: Ray -> Vec3 -> (Face, Vec3)
setFaceNormal r outwardNormal
  | isRayOutside = (Front, outwardNormal)
  | otherwise = (Back, -outwardNormal)
  where
    isRayOutside = dot r.direction outwardNormal < 0
