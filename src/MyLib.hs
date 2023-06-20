module MyLib (someFunc) where

import System.IO (hFlush, hPutStr, stderr)
import Text.Printf

imageWidth :: Int
imageWidth = 256

imageHeight :: Int
imageHeight = 256

-- scaling function to map from [0,width] or [0,height] to [0,1]
scale :: Int -> Int -> Double
scale maxVal val = fromIntegral val / fromIntegral (maxVal - 1)

-- output RGB values scaled to [0,255]
outputPixel :: (Double, Double, Double) -> String
outputPixel (r, g, b) = printf "%d %d %d" (scaleColor r) (scaleColor g) (scaleColor b)
  where
    scaleColor :: Double -> Int
    scaleColor color = truncate (255.999 * color)

-- generate each line of the image
generateLine :: Int -> IO ()
generateLine j = do
  let g = scale imageHeight j
  putStrLn $
    unlines
      [ outputPixel (r, g, 0.25) | i <- [0 .. imageWidth - 1], let r = scale imageWidth i
      ]

-- recursive function to generate image and display progress
generateImage :: Int -> IO ()
generateImage 0 = do
  hPutStr stderr "\rScanlines remaining: 0 \nDone.\n"
  hFlush stderr
generateImage j = do
  hPutStr stderr ("\rScanlines remaining: " ++ show j ++ " ")
  hFlush stderr
  generateLine j
  generateImage (j - 1)

-- image generation
someFunc :: IO ()
someFunc = do
  putStrLn $ printf "P3\n%d %d\n255" imageWidth imageHeight
  generateImage (imageHeight - 1)
