module RtWeekend where

import Camera
import Control.Monad.Primitive
import System.Random.MWC
import System.Random.MWC qualified as MWC
import Vec3
import Prelude hiding (length)

------------------------------------------------------------------------------
-- Constants
------------------------------------------------------------------------------

maximumDepth :: Int
maximumDepth = 50

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

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x : _) = Just x

------------------------------------------------------------------------------
-- Random
------------------------------------------------------------------------------

-- Generates a random value in the interval [0, 1)
randomDouble :: PrimMonad m => Gen (PrimState m) -> m Double
randomDouble g = do
  s <- MWC.uniformR (0, 1) g
  -- subtract this value to get [0, 1) instead of (0,1]
  -- See https://hackage.haskell.org/package/mwc-random-0.15.0.2/docs/src/System.Random.MWC.html#uniform
  pure $ s - 2 ** (-53)

randomInUnitSphere :: PrimMonad m => Gen (PrimState m) -> m Vec3
randomInUnitSphere g = do
  v <- uniformRM (-1, 1 :: Vec3) g
  if lengthSquared v >= 1
    then randomInUnitSphere g
    else pure v
