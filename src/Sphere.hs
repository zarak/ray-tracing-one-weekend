module Sphere where

import Hittable
import Ray
import Vec3

data Sphere = Sphere
  { center :: Point,
    radius :: Double
  }

instance Hittable Sphere where
  hit sphere r tmin tmax hitRecord =
    let oc = sphere.center |-> r.base
        a = dot r.direction r.direction
        halfB = dot oc r.direction
        c = lengthSquared oc - sphere.radius * sphere.radius
        discriminant = halfB * halfB - a * c
        sqrtd = sqrt discriminant
     in _todo
