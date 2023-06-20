module Sphere where

import Hittable
import Ray
import Vec3

data Sphere = Sphere
  { center :: Point,
    radius :: Double
  }

instance Hittable Sphere where
  hit sphere ray (tmin, tmax) = do
    let oc = sphere.center |-> ray.base
        a = dot ray.direction ray.direction
        halfB = dot oc ray.direction
        c = lengthSquared oc - sphere.radius * sphere.radius
        discriminant = halfB * halfB - a * c
        sqrtd = sqrt discriminant
    root <- findNearestRoot halfB sqrtd a (tmin, tmax)
    let t = root
        p = Point $ ray `at` t
        outwardNormal = (sphere.center |-> p) ^/ sphere.radius
        (face, newNormal) = setFaceNormal ray outwardNormal
    pure $ HitRecord {p = p, t = t, normal = newNormal, face = face}

findNearestRoot :: Double -> Double -> Double -> (TMin, TMax) -> Maybe Root
findNearestRoot halfB sqrtd a (tmin, tmax)
  | rootInRange root1 = pure root1
  | rootInRange root2 = pure root2
  | otherwise = Nothing
  where
    root1 = (-halfB - sqrtd) / a
    root2 = (-halfB + sqrtd) / a
    rootInRange root = tmin < root && root < tmax
