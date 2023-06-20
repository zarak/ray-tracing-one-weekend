module MyLib (someFunc) where

import Data.Foldable (traverse_)
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

-- image generation
someFunc :: IO ()
someFunc = do
  putStrLn $ printf "P3\n%d %d\n255" imageWidth imageHeight
  traverse_
    putStrLn
    [ outputPixel (r, g, 0.25) | j <- [imageHeight - 1, imageHeight - 2 .. 0], let g = scale imageHeight j, i <- [0 .. imageWidth - 1], let r = scale imageWidth i
    ]
