{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Color where

import Control.DeepSeq (NFData)
import Data.Text (Text)
import Data.Text.Format (format)
import Data.Text.Lazy (toStrict)
import GHC.Generics (Generic)
import RtWeekend
import Vec3

type Red = Double

type Green = Double

type Blue = Double

newtype Color = Color
  { toVec3 :: Vec3
  }
  deriving (Show, Num)

deriving instance Generic Color

deriving instance NFData Color

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

scaleColor :: Double -> Color -> Color
scaleColor t c = Color $ t *^ c.toVec3

writeColor :: Color -> Int -> Text
writeColor (Color (Vec3 r g b)) samplesPerPixel =
  -- Divide by the number of ray samples
  let scale = 1.0 / fromIntegral samplesPerPixel
      -- Square root is to gamma-correct for gamma=2.0
      r' = sqrt $ r * scale
      g' = sqrt $ g * scale
      b' = sqrt $ b * scale
      toColor comp = floor (256 * clamp comp 0.0 0.999) :: Int
   in toStrict $ format "{} {} {}" (toColor r', toColor g', toColor b')
