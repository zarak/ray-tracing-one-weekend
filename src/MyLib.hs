module MyLib (someFunc) where

import Color (writeColor)
import Ray
import RtWeekend
import System.IO (hFlush, hPutStr, stderr)
import Text.Printf
import Vec3

rayColor :: Ray -> Color
rayColor r =
  let unitDirection = unitVector r.direction
      t = 0.5 * unitDirection.y + 1.0
      blue = color 0.5 0.7 1.0
   in Color $ (1.0 - t) *^ white.toVec3 + t *^ blue.toVec3

-- scaling function to map from [0,width] or [0,height] to [0,1]
scale :: Int -> Int -> Double
scale maxVal val = fromIntegral val / fromIntegral (maxVal - 1)

generateLine :: Int -> IO ()
generateLine j = do
  let g = scale imageHeight j
  putStrLn $
    unlines
      [ writeColor (r, g, 0.25) | i <- [0 .. imageWidth - 1], let r = scale imageWidth i
      ]

-- function to generate image and display progress
generateImage :: Int -> IO ()
generateImage 0 = do
  hPutStr stderr "\rScanlines remaining: 0 \nDone.\n"
  hFlush stderr
generateImage j = do
  hPutStr stderr ("\rScanlines remaining: " ++ show j ++ "\n")
  hFlush stderr
  generateLine j
  generateImage (j - 1)

someFunc :: IO ()
someFunc = do
  putStrLn $ printf "P3\n%d %d\n255" imageWidth imageHeight
  generateImage (imageHeight - 1)
