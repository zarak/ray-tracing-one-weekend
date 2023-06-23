{-# LANGUAGE DuplicateRecordFields #-}

module Camera where

import Ray
import RtWeekend
import Vec3

data Camera = Camera
  { origin :: Point,
    horizontal :: Vec3,
    vertical :: Vec3,
    lowerLeftCorner :: Vec3,
    config :: CameraConfig
  }
  deriving (Show)

data CameraConfig = CameraConfig
  { u :: Vec3,
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
      cameraConfig = CameraConfig {u = u, v = v, w = w, lensRadius = lensRadius}
   in Camera
        { origin = origin,
          horizontal = horizontal,
          vertical = vertical,
          lowerLeftCorner = lowerLeftCorner,
          config = cameraConfig
        }

getRay :: Camera -> Double -> Double -> Vec3 -> Ray
getRay (Camera origin horizontal vertical lowerLeftCorner (CameraConfig u v w lensRadius)) s t randomVec =
  let rd = lensRadius *^ randomVec
      offset = u ^* rd.x + v ^* rd.y
   in Ray
        (Point $ origin.toVec3 + offset)
        ( ( lowerLeftCorner
              + s
              *^ horizontal
              + t
              *^ vertical
          )
            - origin.toVec3
            - offset
        )
