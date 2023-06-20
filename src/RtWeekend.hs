module RtWeekend where

import Camera
import Color

------------------------------------------------------------------------------
-- Image
------------------------------------------------------------------------------
imageWidth :: Int
imageWidth = 400

imageHeight :: Int
imageHeight = truncate $ fromIntegral imageWidth / defaultCamera.aspectRatio

white :: Color
white = color 1 1 1

black :: Color
black = color 0 0 0

------------------------------------------------------------------------------
-- Utility
------------------------------------------------------------------------------
degreesToRadians :: Double -> Double
degreesToRadians deg = deg * pi / 180

clamp :: Double -> Double -> Double -> Double
clamp x min' max'
  | x < min' = min'
  | x > max' = max'
  | otherwise = x
