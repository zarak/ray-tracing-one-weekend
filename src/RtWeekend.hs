module RtWeekend where

import Camera
import Control.Monad.Primitive
import System.Random.MWC
import System.Random.MWC qualified as MWC

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

-- Generates a random value in the interval [0, 1)
randomDouble :: PrimMonad m => Gen (PrimState m) -> m Double
randomDouble g = do
  s <- MWC.uniformR (0, 1) g
  pure $ s - 2 ** (-53) -- subtract this value to get [0, 1) instead of (0,1]
