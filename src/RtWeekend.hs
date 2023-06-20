module RtWeekend where

import Camera

------------------------------------------------------------------------------
-- Image
------------------------------------------------------------------------------
imageWidth :: Int
imageWidth = 400

imageHeight :: Int
imageHeight = truncate $ fromIntegral imageWidth / defaultCamera.aspectRatio

------------------------------------------------------------------------------
-- Utility
------------------------------------------------------------------------------
degreesToRadians :: Double -> Double
degreesToRadians deg = deg * pi / 180

clamp :: Double -> Double -> Double -> Double
clamp x low high = max low (min high x)

------------------------------------------------------------------------------
-- Random
------------------------------------------------------------------------------
