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

rayColor :: Ray -> Gen (PrimState IO) -> Int -> IO (World Sphere) -> IO Color
rayColor _ _ 0 _ = pure mempty
rayColor r g depth worldIO = do
  let unitDirection = unitVector r.direction
      t = 0.5 * (unitDirection.y + 1.0)
      blue = color 0.5 0.7 1.0
      blueWhiteLerp = scaleColor (1.0 - t) white <> scaleColor t blue
  world <- worldIO

  case hit world r (shadowAcne, fromRational infinity) of
    Nothing -> pure blueWhiteLerp
    Just rec -> do
      case rec.material.scatter r rec of
        Nothing -> pure mempty
        Just scattered -> do
          c <- rayColor scattered.ray g (depth - 1) (pure world)
          pure $ Color $ scattered.attenuation.toVec3 * c.toVec3

generateLine :: Int -> Gen (PrimState IO) -> IO (World Sphere) -> IO ()
generateLine j g world = do
  sampledColors <- forM [1 .. imageWidth] $ \i -> do
    cs <- replicateM samplesPerPixel $ do
      drawRay i j g world
    let summedColors = foldr (<>) mempty cs
    pure $ writeColor summedColors samplesPerPixel
  T.putStrLn $ T.unlines sampledColors

drawRay :: Int -> Int -> Gen (PrimState IO) -> IO (World Sphere) -> IO Color
drawRay i j g world = do
  x <- randomDouble g
  y <- randomDouble g
  let u = (fromIntegral i + x) / fromIntegral (imageWidth - 1)
      v = (fromIntegral j + y) / fromIntegral (imageHeight - 1)
      r = getRay u v
      pixelColor = rayColor r g maximumDepth world
   in pixelColor

-- Generate image (without header) and display progress
generateImage :: Int -> Gen (PrimState IO) -> IO (World Sphere) -> IO ()
generateImage 0 _ _ = do
  hPutStr stderr "\rScanlines remaining: 0 \nDone.\n"
  hFlush stderr
generateImage j g world = do
  hPutStr stderr ("\rScanlines remaining: " ++ show j ++ "\n")
  hFlush stderr
  generateLine j g world
  generateImage (j - 1) g world

-- Generate image with header
someFunc :: IO ()
someFunc = do
  -- g <- MWC.createSystemRandom
  g <- MWC.create -- use for testing
  putStrLn $ printf "P3\n%d %d\n255" imageWidth imageHeight
  let materialGround = lambertian (color 0.8 0.8 0) g
      materialCenter = lambertian (color 0.7 0.3 0.3) g
      materialLeft = metal (color 0.8 0.8 0.8) g
      materialRight = metal (color 0.8 0.6 0.2) g
      sphere1 = Sphere (point 0.0 -100.5 -1.0) 100.0 <$> materialGround :: IO Sphere
      sphere2 = Sphere (point 0.0 0.0 -1.0) 0.5 <$> materialCenter :: IO Sphere
      sphere3 = Sphere (point -1.0 0.0 -1.0) 0.5 <$> materialLeft :: IO Sphere
      sphere4 = Sphere (point 1.0 0.0 -1.0) 0.5 <$> materialRight :: IO Sphere
      world = mkWorld [sphere1, sphere2, sphere3, sphere4]
  generateImage imageHeight g world

mkWorld :: (PrimMonad m, Hittable a) => [m a] -> m (World a)
mkWorld xs = World <$> sequence xs
