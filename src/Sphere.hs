module Sphere where

import Hittable
import Ray
import Vec3

data Sphere = Sphere
  { center :: Point,
    radius :: Double
  }

instance Hittable Sphere where
  hit sphere r tmin tmax = do
    let oc = sphere.center |-> r.base
        a = dot r.direction r.direction
        halfB = dot oc r.direction
        c = lengthSquared oc - sphere.radius * sphere.radius
        discriminant = halfB * halfB - a * c
        sqrtd = sqrt discriminant
    root <- findNearestRoot halfB sqrtd a tmin tmax
    let t = root
        p = Point $ r `at` t
        outwardNormal = (sphere.center |-> p) ^/ sphere.radius
        (face, newNormal) = setFaceNormal r outwardNormal
    pure $ HitRecord {p = p, t = t, normal = newNormal, face = face}

findNearestRoot :: Double -> Double -> Double -> Double -> Double -> Maybe Root
findNearestRoot halfB sqrtd a tmin tmax
  | rootInRange root1 = pure root1
  | not (rootInRange root1) && rootInRange root2 = pure root2
  | noRoots = Nothing
  | otherwise = Nothing
  where
    root1 = (-halfB - sqrtd) / a
    root2 = (-halfB + sqrtd) / a
    rootInRange root = tmin < root && root < tmax
    noRoots = not (rootInRange root1 || rootInRange root2)
