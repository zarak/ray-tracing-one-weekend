{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fold" #-}
module MyLib (someFunc) where

import Camera
import Color (Color (..), color, scaleColor, white, writeColor)
import Control.Monad (forM, replicateM)
import Control.Monad.Primitive
import GHC.Real (infinity)
import Hittable
import Ray
import RtWeekend
import Sphere
import System.IO (hFlush, hPutStr, stderr)
import System.Random.MWC
import System.Random.MWC qualified as MWC
import Text.Printf
import Vec3

samplesPerPixel :: Int
samplesPerPixel = 100

shadowAcne :: Double
shadowAcne = 0.001

world :: World Sphere
world =
  let sphere1 = Sphere (point 0 0 -1) 0.5
      sphere2 = Sphere (point 0 -100.5 -1) 100
   in World [sphere1, sphere2]

rayColor :: PrimMonad m => Ray -> Gen (PrimState m) -> Int -> m Color
rayColor r g depth = do
  let unitDirection = unitVector r.direction
      t = 0.5 * (unitDirection.y + 1.0)
      blue = color 0.5 0.7 1.0
      blueWhiteLerp = Color $ (1.0 - t) *^ white.toVec3 + t *^ blue.toVec3
  case hit world r (shadowAcne, fromRational infinity) of
    Nothing -> pure blueWhiteLerp
    Just rec -> do
      randomUnitVec <- unitVector <$> randomInUnitSphere g
      let target = rec.p.toVec3 + rec.normal + randomUnitVec
          newRay = Ray rec.p (target - rec.p.toVec3)
      newColor <- rayColor newRay g (depth - 1)
      pure $ scaleColor 0.5 newColor

-- Generate the line of a PPM format image
generateLine :: Int -> Gen (PrimState IO) -> IO ()
generateLine j g = do
  sampledColors <- forM [1 .. imageWidth] $ \i -> do
    cs <- replicateM samplesPerPixel $ do
      drawRay i j g
    let summedColors = foldr (<>) mempty cs
    pure $ writeColor summedColors samplesPerPixel
  putStrLn $ unlines sampledColors

drawRay :: PrimMonad m => Int -> Int -> Gen (PrimState m) -> m Color
drawRay i j g = do
  x <- randomDouble g
  y <- randomDouble g
  let u = (fromIntegral i + x) / fromIntegral (imageWidth - 1)
      v = (fromIntegral j + y) / fromIntegral (imageHeight - 1)
      r = getRay u v
      pixelColor = rayColor r g maximumDepth
   in pixelColor

-- Generate image (without header) and display progress
generateImage :: Int -> Gen (PrimState IO) -> IO ()
generateImage 0 _ = do
  hPutStr stderr "\rScanlines remaining: 0 \nDone.\n"
  hFlush stderr
generateImage j g = do
  hPutStr stderr ("\rScanlines remaining: " ++ show j ++ "\n")
  hFlush stderr
  generateLine j g
  generateImage (j - 1) g

-- Generate image with header
someFunc :: IO ()
someFunc = do
  g <- MWC.createSystemRandom
  putStrLn $ printf "P3\n%d %d\n255" imageWidth imageHeight
  generateImage imageHeight g
