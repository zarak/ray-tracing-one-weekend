module Color where

import Text.Printf (printf)

-- output RGB values scaled to [0,255]
writeColor :: (Double, Double, Double) -> String
writeColor (r, g, b) = printf "%d %d %d" (scaleColor r) (scaleColor g) (scaleColor b)
  where
    scaleColor :: Double -> Int
    scaleColor color = truncate (255.999 * color)
