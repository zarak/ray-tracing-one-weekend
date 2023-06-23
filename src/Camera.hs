{-# LANGUAGE DuplicateRecordFields #-}

module Camera where

import Ray
import RtWeekend
import Vec3

------------------------------------------------------------------------------
-- Image
------------------------------------------------------------------------------

data Camera = Camera
  { origin :: Point,
    horizontal :: Vec3,
    vertical :: Vec3,
    lowerLeftCorner :: Vec3
  }
  deriving (Show)

mkCamera :: Double -> Double -> Camera
mkCamera vfov aspectRatio =
  let theta = degreesToRadians vfov
      h = tan $ theta / 2
      viewportHeight = 2.0 * h
      viewportWidth = aspectRatio * viewportHeight

      focalLength = 1.0

      origin = Point zeros
      horizontal = Vec3 viewportWidth 0 0
      vertical = Vec3 0 viewportHeight 0
      lowerLeftCorner =
        origin.toVec3
          - horizontal ^/ 2
          - vertical ^/ 2
          - Vec3 0 0 focalLength
   in Camera
        { origin = origin,
          horizontal = horizontal,
          vertical = vertical,
          lowerLeftCorner = lowerLeftCorner
        }

getRay :: Camera -> Double -> Double -> Ray
getRay camera u v =
  Ray
    camera.origin
    -- The direction vector of the ray goes from the origin of the camera to
    -- a point on the screen with coordinates (u, v)
    -- See https://raytracing.github.io/images/fig-1.03-cam-geom.jpg
    ( camera.origin
        |-> Point
          ( camera.lowerLeftCorner
              + u
              *^ camera.horizontal
              + v
              *^ camera.vertical
          )
    )
