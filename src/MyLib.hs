{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fold" #-}
module MyLib (someFunc, rayColor, generateLine) where

import Camera
import Color (Color (..), color, scaleColor, white, writeColor)
import Control.Monad (forM, replicateM)
import Control.Monad.Primitive
import Data.Text qualified as T
import Data.Text.IO qualified as T (putStrLn)
import GHC.Real (infinity)
import Hittable
import Material
import MaterialHit
import Ray
import RtWeekend
import Sphere
import System.IO (hFlush, hPutStr, stderr)
import System.Random.MWC
import System.Random.MWC qualified as MWC
import Text.Printf
import Vec3

samplesPerPixel :: Int
samplesPerPixel = 100

shadowAcne :: Double
shadowAcne = 0.001

world :: PrimMonad m => Gen (PrimState m) -> m (World Sphere)
world g = do
  materialGround <- lambertian (color 0.8 0.8 0) g
  let sphere1 = Sphere (point 0.0 -100.5 -1.0) 100.0 materialGround
  pure $ World [sphere1]

rayColor :: PrimMonad m => Ray -> Gen (PrimState m) -> Int -> m Color
rayColor _ _ 0 = pure mempty
rayColor r g depth = do
  world' <- world g
  let unitDirection = unitVector r.direction
      t = 0.5 * (unitDirection.y + 1.0)
      blue = color 0.5 0.7 1.0
      blueWhiteLerp = scaleColor (1.0 - t) white <> scaleColor t blue
  case hit world' r (shadowAcne, fromRational infinity) of
    Nothing -> pure blueWhiteLerp
    Just rec -> do
      case rec.material.scatter r rec of
        Nothing -> pure white
        Just s -> do
          c <- rayColor s.scattered g (depth - 1)
          pure $ Color $ s.attenuation.toVec3 * c.toVec3

generateLine :: Int -> Gen (PrimState IO) -> IO ()
generateLine j g = do
  sampledColors <- forM [1 .. imageWidth] $ \i -> do
    cs <- replicateM samplesPerPixel $ do
      drawRay i j g
    let summedColors = foldr (<>) mempty cs
    pure $ writeColor summedColors samplesPerPixel
  T.putStrLn $ T.unlines sampledColors

drawRay :: PrimMonad m => Int -> Int -> Gen (PrimState m) -> m Color
drawRay i j g = do
  x <- randomDouble g
  y <- randomDouble g
  let u = (fromIntegral i + x) / fromIntegral (imageWidth - 1)
      v = (fromIntegral j + y) / fromIntegral (imageHeight - 1)
      r = getRay u v
      pixelColor = rayColor r g maximumDepth
   in pixelColor

-- Generate image (without header) and display progress
generateImage :: Int -> Gen (PrimState IO) -> IO ()
generateImage 0 _ = do
  hPutStr stderr "\rScanlines remaining: 0 \nDone.\n"
  hFlush stderr
generateImage j g = do
  hPutStr stderr ("\rScanlines remaining: " ++ show j ++ "\n")
  hFlush stderr
  generateLine j g
  generateImage (j - 1) g

-- Generate image with header
someFunc :: IO ()
someFunc = do
  -- g <- MWC.createSystemRandom
  g <- MWC.create -- use for testing
  putStrLn $ printf "P3\n%d %d\n255" imageWidth imageHeight
  generateImage imageHeight g
