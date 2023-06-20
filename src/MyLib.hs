module MyLib (someFunc) where

import Color (Color (..), color, writeColor)
import GHC.Real (infinity)
import Hittable
import Ray
import RtWeekend
import Sphere
import System.IO (hFlush, hPutStr, stderr)
import Text.Printf
import Vec3

rayColor :: Hittable a => Ray -> a -> Color
rayColor r world =
  let unitDirection = unitVector r.direction
      missT = 0.5 * unitDirection.y + 1.0
      blue = color 0.5 0.7 1.0
      blueWhiteLerp = Color $ (1.0 - missT) *^ white.toVec3 + missT *^ blue.toVec3
   in case hit world r (0, fromRational infinity) of
        Nothing -> blueWhiteLerp
        Just hitRecord -> do
          Color $ unitToInterval hitRecord.normal

-- Generate the line of a PPM format image
generateLine :: Hittable a => a -> Int -> IO ()
generateLine world j = do
  putStrLn $
    unlines
      [ writeColor c | i <- [0 .. imageWidth - 1], let c = drawRay world i j
      ]

drawRay :: Hittable a => a -> Int -> Int -> Color
drawRay world i j =
  let u = fromIntegral i / fromIntegral (imageWidth - 1)
      v = fromIntegral j / fromIntegral (imageHeight - 1)
      r = Ray origin (lowerLeftCorner + u *^ horizontal + v *^ vertical - origin.toVec3)
      pixelColor = rayColor r world
   in pixelColor

-- Generate image (without header) and display progress
generateImage :: Hittable a => a -> Int -> IO ()
generateImage _ 0 = do
  hPutStr stderr "\rScanlines remaining: 0 \nDone.\n"
  hFlush stderr
generateImage world j = do
  hPutStr stderr ("\rScanlines remaining: " ++ show j ++ "\n")
  hFlush stderr
  generateLine world j
  generateImage world (j - 1)

-- Generate image with header
someFunc :: IO ()
someFunc = do
  let sphere1 = Sphere (point 0 0 -1) 0.5
      sphere2 = Sphere (point 0 -100.5 -1) 100
      -- sphere3 = Sphere (point 0 100.5 -1) 100
      world = HittableList [sphere1, sphere2]
  putStrLn $ printf "P3\n%d %d\n255" imageWidth imageHeight
  generateImage world imageHeight
