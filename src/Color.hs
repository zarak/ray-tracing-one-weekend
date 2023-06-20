module Color where

import Text.Printf (printf)
import Vec3

-- output RGB values scaled to [0,255]
writeColor :: Color -> String
writeColor (Color v) = printf "%d %d %d" (scaleColor v.x) (scaleColor v.y) (scaleColor v.z)
  where
    scaleColor :: Double -> Int
    scaleColor color' = truncate (255.999 * color')
