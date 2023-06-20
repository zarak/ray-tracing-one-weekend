module Color where

import Text.Printf (printf)
import Vec3

type Red = Double

type Green = Double

type Blue = Double

newtype Color = Color
  { toVec3 :: Vec3
  }
  deriving (Show, Num)

-- output RGB values scaled to [0,255]
writeColor :: Color -> String
writeColor (Color v) = printf "%d %d %d" (scaleColor v.x) (scaleColor v.y) (scaleColor v.z)
  where
    scaleColor :: Double -> Int
    scaleColor color' = truncate (255.999 * color')

color :: Red -> Green -> Blue -> Color
color a b c = Color $ Vec3 a b c
