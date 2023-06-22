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
    if discriminant < 0
      then Nothing
      else pure $ HitRecord {p = p, t = t, normal = newNormal, face = face}

findNearestRoot :: Double -> Double -> Double -> (TMin, TMax) -> Maybe Root
findNearestRoot halfB sqrtd a (tmin, tmax)
  | rootInRange root1 = pure root1
  | rootInRange root2 = pure root2
  | otherwise = Nothing
  where
    root1 = (-halfB - sqrtd) / a
    root2 = (-halfB + sqrtd) / a
    rootInRange root = tmin < root && root < tmax

newtype World a = World [a]

instance Hittable a => Hittable (World a) where
  hit (World []) _ _ = Nothing
  hit (World (h : hs)) ray range =
    case hit h ray range of
      -- If there are no hits on this object, check the rest of the objects in
      -- the world with the same range
      Nothing -> hit (World hs) ray range
      -- If there is a hit, reduce the range and check the remaining objects to
      -- see if a ray hits a closer object
      mHitRecord@(Just hitRecord) ->
        let (tmin, _) = range
         in maybe
              mHitRecord -- no closer objects found, so return this object
              pure
              (hit (World hs) ray (tmin, hitRecord.t))
