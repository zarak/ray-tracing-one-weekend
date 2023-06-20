module RtWeekend where

import Vec3

------------------------------------------------------------------------------
-- Image
------------------------------------------------------------------------------
aspectRatio :: Double
aspectRatio = 16.0 / 9.0

imageWidth :: Int
imageWidth = 400

imageHeight :: Int
imageHeight = truncate $ fromIntegral imageWidth / aspectRatio

white :: Color
white = color 1 1 1

------------------------------------------------------------------------------
-- Camera
------------------------------------------------------------------------------
viewportHeight :: Double
viewportHeight = 2.0

viewportWidth :: Double
viewportWidth = aspectRatio * viewportHeight

focalLength :: Double
focalLength = 1.0

origin :: Point
origin = Point zeros

black :: Color
black = color 0 0 0

horizontal :: Vec3
horizontal = Vec3 viewportWidth 0 0

vertical :: Vec3
vertical = Vec3 0 viewportHeight 0

lowerLeftCorner :: Vec3
lowerLeftCorner =
  origin.toVec3
    - horizontal ^/ 2
    - vertical ^/ 2
    - Vec3 0 0 focalLength