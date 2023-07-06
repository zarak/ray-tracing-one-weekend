module Hittable where

import MaterialHit
import Ray
import Vec3

type TMin = Double

type TMax = Double

type Root = Double

class Hittable a where
  hit :: a -> Ray -> (TMin, TMax) -> Maybe HitRecord

-- We use the convention that normals always point against the incident ray.
-- If the dot product is negative, the ray is outside the sphere so we return a
-- (Front, +ve) tuple. If it's positive, we negate the normal so that it points
-- against the incident ray, thus returning (Back, -ve).
setFaceNormal :: Ray -> Vec3 -> (Face, Vec3)
setFaceNormal r outwardNormal
  | isRayOutside = (Front, outwardNormal)
  | otherwise = (Back, -outwardNormal)
  where
    isRayOutside = dot r.direction outwardNormal < 0

data AnyHittable = forall a. Hittable a => AnyHittable a

instance Hittable AnyHittable where
  hit (AnyHittable a) = hit a
