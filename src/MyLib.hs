module MyLib (someFunc) where

import Color (Color (..), color, writeColor)
import Ray
import RtWeekend
import System.IO (hFlush, hPutStr, stderr)
import Text.Printf
import Vec3

type Radius = Double

-- oc: vector from the center of the circle to the base of the ray
hitSphere :: Point -> Radius -> Ray -> Bool
hitSphere center radius r =
  let oc = r.base.toVec3 - center.toVec3
      a = dot r.direction r.direction
      b = 2.0 * dot oc r.direction
      c = dot oc oc - radius * radius
      discriminant = b * b - 4 * a * c
   in discriminant > 0

rayColor :: Ray -> Color
rayColor r =
  let unitDirection = unitVector r.direction
      t = 0.5 * unitDirection.y + 1.0
      blue = color 0.5 0.7 1.0
      circleCenter = point 0 0 -1
      circleRadius = 0.5
   in if hitSphere circleCenter circleRadius r
        then color 1 0 0
        else Color $ (1.0 - t) *^ white.toVec3 + t *^ blue.toVec3

-- Generate the line of a PPM format image
generateLine :: Int -> IO ()
generateLine j = do
  putStrLn $
    unlines
      [ writeColor c | i <- [0 .. imageWidth - 1], let c = drawRay i j
      ]

drawRay :: Int -> Int -> Color
drawRay i j =
  let u = fromIntegral i / fromIntegral (imageWidth - 1)
      v = fromIntegral j / fromIntegral (imageHeight - 1)
      r = Ray origin (lowerLeftCorner + u *^ horizontal + v *^ vertical - origin.toVec3)
      pixelColor = rayColor r
   in pixelColor

-- Generate image (without header) and display progress
generateImage :: Int -> IO ()
generateImage 0 = do
  hPutStr stderr "\rScanlines remaining: 0 \nDone.\n"
  hFlush stderr
generateImage j = do
  hPutStr stderr ("\rScanlines remaining: " ++ show j ++ "\n")
  hFlush stderr
  generateLine j
  generateImage (j - 1)

-- Generate image with header
someFunc :: IO ()
someFunc = do
  putStrLn $ printf "P3\n%d %d\n255" imageWidth imageHeight
  generateImage imageHeight
