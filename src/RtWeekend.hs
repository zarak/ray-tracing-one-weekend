module RtWeekend where

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
randomDoubleR :: PrimMonad m => Double -> Double -> Gen (PrimState m) -> m Double
randomDoubleR lo hi g = do
  s <- MWC.uniformR (lo, hi) g
  -- subtract this value to get [lo, hi) instead of (lo,hi]
  -- See https://hackage.haskell.org/package/mwc-random-0.15.0.2/docs/src/System.Random.MWC.html#uniform
  pure $ s - 2 ** (-53)

-- Generates a random value in the interval [0, 1)
randomDouble :: PrimMonad m => Gen (PrimState m) -> m Double
randomDouble g = do
  randomDoubleR 0 1 g

randomInUnitSphere :: PrimMonad m => Gen (PrimState m) -> m Vec3
randomInUnitSphere g = do
  v <- uniformRM (-1, 1 :: Vec3) g
  if lengthSquared v >= 1
    then randomInUnitSphere g
    else pure v

randomInHemisphere :: PrimMonad m => Vec3 -> Gen (PrimState m) -> m Vec3
randomInHemisphere normal g = do
  inUnitSphere <- randomInUnitSphere g
  if dot inUnitSphere normal > 0.0 then pure inUnitSphere else pure (-inUnitSphere)

randomInUnitDisk :: PrimMonad m => Gen (PrimState m) -> m Vec3
randomInUnitDisk g = do
  x <- randomDoubleR -1 1 g
  y <- randomDoubleR -1 1 g
  let p = Vec3 x y 0
  if lengthSquared p >= 1
    then randomInUnitSphere g
    else pure p
