{-# LANGUAGE DuplicateRecordFields #-}

module MovingSphere where

import Hittable
import MaterialHit (HitRecord (..), Material)
import Ray
import Sphere (findNearestRoot)
import Vec3

data MovingSphere = MovingSphere
  { center0 :: Point,
    center1 :: Point,
    time0 :: Double,
    time1 :: Double,
    radius :: Double,
    material :: Material
  }

center :: MovingSphere -> Double -> Point
center ms time =
  Point $
    ms.center0.toVec3
      + ((time - ms.time0) / (ms.time1 - ms.time0))
      *^ (ms.center0 |-> ms.center1)

instance Hittable MovingSphere where
  {-# INLINEABLE hit #-}
  hit ms ray (tmin, tmax) = do
    let oc = (center ms ray.time) |-> ray.base
        a = dot ray.direction ray.direction
        halfB = dot oc ray.direction
        c = lengthSquared oc - ms.radius * ms.radius
        discriminant = halfB * halfB - a * c
        sqrtd = sqrt discriminant
    root <- findNearestRoot halfB sqrtd a (tmin, tmax)
    let t = root
        p = Point $ ray `at` t
        outwardNormal = ((center ms ray.time) |-> p) ^/ ms.radius
        (face, newNormal) = setFaceNormal ray outwardNormal
    if discriminant < 0
      then Nothing
      else
        pure $
          HitRecord
            { p = p,
              t = t,
              normal = newNormal,
              face = face,
              material = ms.material
            }
