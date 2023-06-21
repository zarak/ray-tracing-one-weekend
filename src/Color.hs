module Color where

import RtWeekend
import Text.Printf (printf)
import Vec3

type Red = Double

type Green = Double

type Blue = Double

newtype Color = Color
  { toVec3 :: Vec3
  }
  deriving (Show, Num)

instance Semigroup Color where
  c1 <> c2 = Color $ c1.toVec3 + c2.toVec3

instance Monoid Color where
  mempty = black

white :: Color
white = color 1 1 1

black :: Color
black = color 0 0 0

color :: Red -> Green -> Blue -> Color
color a b c = Color $ Vec3 a b c

writeColor :: Color -> Int -> String
writeColor (Color (Vec3 r g b)) samplesPerPixel =
  let scale = 1.0 / fromIntegral samplesPerPixel
      r' = r * scale
      g' = g * scale
      b' = b * scale
      toColor comp = floor (256 * clamp comp 0.0 0.999) :: Int
   in printf "%d %d %d" (toColor r') (toColor g') (toColor b')
