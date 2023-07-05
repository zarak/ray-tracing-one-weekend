{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Camera where

import Ray
import RtWeekend
import Vec3

data Camera = Camera
  { origin :: Point,
    horizontal :: Vec3,
    vertical :: Vec3,
    lowerLeftCorner :: Vec3,
    -- u, v, w are basis vectors
    u :: Vec3,
    v :: Vec3,
    w :: Vec3,
    lensRadius :: Double
  }
  deriving (Show)

mkCamera :: Point -> Point -> Vec3 -> Double -> Double -> Double -> Double -> Camera
mkCamera lookfrom lookat vup vfov aspectRatio aperture focusDist =
  let theta = degreesToRadians vfov
      h = tan $ theta / 2
      viewportHeight = 2.0 * h
      viewportWidth = aspectRatio * viewportHeight

      w = unitVector $ lookat |-> lookfrom
      u = unitVector $ cross vup w
      v = cross w u

      origin = lookfrom
      horizontal = focusDist *^ viewportWidth *^ u
      vertical = focusDist *^ viewportHeight *^ v
      lowerLeftCorner =
        origin.toVec3
          - horizontal
          ^/ 2
          - vertical
          ^/ 2
          - focusDist
          *^ w

      lensRadius = aperture / 2
   in Camera {..}

cameraRay :: Camera -> Double -> Double -> Vec3 -> Ray
cameraRay Camera {..} s t randomInUnitDisk_ =
  let rd = lensRadius *^ randomInUnitDisk_
      offset = u ^* rd.x + v ^* rd.y
   in Ray
        { base = Point $ origin.toVec3 + offset,
          direction =
            ( lowerLeftCorner
                + s
                *^ horizontal
                + t
                *^ vertical
            )
              - origin.toVec3
              - offset
        }
