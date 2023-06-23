{-# LANGUAGE DuplicateRecordFields #-}

module Camera where

import Ray
import RtWeekend
import Vec3

data Camera = Camera
  { origin :: Point,
    horizontal :: Vec3,
    vertical :: Vec3,
    lowerLeftCorner :: Vec3
  }
  deriving (Show)

mkCamera :: Point -> Point -> Vec3 -> Double -> Double -> Camera
mkCamera lookfrom lookat vup vfov aspectRatio =
  let theta = degreesToRadians vfov
      h = tan $ theta / 2
      viewportHeight = 2.0 * h
      viewportWidth = aspectRatio * viewportHeight

      w = unitVector $ lookat |-> lookfrom
      u = unitVector $ cross vup w
      v = cross w u

      origin = lookfrom
      horizontal = viewportWidth *^ u
      vertical = viewportHeight *^ v
      lowerLeftCorner =
        origin.toVec3
          - horizontal ^/ 2
          - vertical ^/ 2
          - w
   in Camera
        { origin = origin,
          horizontal = horizontal,
          vertical = vertical,
          lowerLeftCorner = lowerLeftCorner
        }

getRay :: Camera -> Double -> Double -> Ray
getRay camera s t =
  Ray
    camera.origin
    ( camera.origin
        |-> Point
          ( camera.lowerLeftCorner
              + s
              *^ camera.horizontal
              + t
              *^ camera.vertical
          )
    )
