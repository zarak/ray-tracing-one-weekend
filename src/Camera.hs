{-# LANGUAGE DuplicateRecordFields #-}

module Camera where

import Vec3

data Camera = Camera
  { aspectRatio :: Double,
    viewportHeight :: Double,
    viewportWidth :: Double,
    focalLength :: Double,
    origin :: Point,
    horizontal :: Vec3,
    vertical :: Vec3,
    lowerLeftCorner :: Vec3
  }
  deriving (Show)

defaultViewportWidth :: Double
defaultViewportWidth = defaultAspectRatio * defaultViewportHeight

defaultViewportHeight :: Double
defaultViewportHeight = 2.0

defaultAspectRatio :: Double
defaultAspectRatio = 16.0 / 9.0

defaultHorizontal :: Vec3
defaultHorizontal = Vec3 defaultViewportWidth 0 0

defaultVertical :: Vec3
defaultVertical = Vec3 0 defaultViewportHeight 0

defaultFocalLength :: Double
defaultFocalLength = 1.0

defaultOrigin :: Point
defaultOrigin = Point zeros

defaultCamera :: Camera
defaultCamera =
  Camera
    { aspectRatio = defaultAspectRatio,
      viewportHeight = defaultViewportHeight,
      viewportWidth = defaultViewportWidth,
      focalLength = defaultFocalLength,
      origin = defaultOrigin,
      horizontal = defaultHorizontal,
      vertical = defaultVertical,
      lowerLeftCorner =
        defaultOrigin.toVec3
          - defaultHorizontal ^/ 2
          - defaultVertical ^/ 2
          - Vec3 0 0 defaultFocalLength
    }
