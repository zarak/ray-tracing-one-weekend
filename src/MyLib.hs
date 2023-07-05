{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fold" #-}
{-# HLINT ignore "Redundant pure" #-}
module MyLib (someFunc, rayColor, generateLine) where

import Camera
import Color (Color (..), color, scaleColor, white, writeColor)
import Control.Monad (forM, replicateM)
import Control.Monad.Primitive
-- import Control.Parallel.Strategies (parBuffer, rdeepseq, rseq, withStrategy)
import Data.Maybe (catMaybes)
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

aspectRatio :: Double
aspectRatio = 16.0 / 9.0

imageWidth :: Int
imageWidth = 200

imageHeight :: Int
imageHeight = truncate $ fromIntegral imageWidth / aspectRatio

camera :: Camera
-- camera = mkCamera (point -2 2 1) (point 0 0 -1) (Vec3 0 1 0) 90.0 aspectRatio
camera =
  mkCamera
    lookfrom
    lookat
    vup
    20
    aspectRatio
    aperture
    distToFocus
  where
    lookfrom = point 13 2 3
    lookat = Point zeros
    vup = Vec3 0 1 0
    distToFocus = 10.0
    aperture = 0.1

rayColor :: Ray -> GenIO -> Int -> World Sphere -> IO Color
rayColor _ _ 0 _ = pure mempty
rayColor r g depth world = do
  let unitDirection = unitVector r.direction
      t = 0.5 * (unitDirection.y + 1.0)
      blue = color 0.5 0.7 1.0
      blueWhiteLerp = scaleColor (1.0 - t) white <> scaleColor t blue

  case hit world r (shadowAcne, fromRational infinity) of
    Nothing -> pure blueWhiteLerp
    Just rec -> do
      x <- rec.material.scatter g r rec
      case x of
        Nothing -> pure mempty
        Just scattered -> do
          c <- rayColor scattered.ray g (depth - 1) world
          pure $ Color $ scattered.attenuation.toVec3 * c.toVec3

drawRay :: Int -> Int -> GenIO -> World Sphere -> IO Color
drawRay i j g world = do
  x <- randomDouble g
  y <- randomDouble g
  randomInDisk <- randomInUnitDisk g
  let u = (fromIntegral i + x) / fromIntegral (imageWidth - 1)
      v = (fromIntegral j + y) / fromIntegral (imageHeight - 1)
      r = cameraRay camera u v randomInDisk
  pixelColor <- rayColor r g maximumDepth world
  pure pixelColor

generateLine :: Int -> Gen (PrimState IO) -> World Sphere -> IO ()
generateLine j g world = do
  sampledColors <- forM [1 .. imageWidth] $ \i -> do
    cs <- replicateM samplesPerPixel $ drawRay i j g world
    let summedColors = foldr (<>) mempty cs
    pure $ writeColor summedColors samplesPerPixel
  T.putStrLn $ T.unlines sampledColors

generateImage :: Int -> GenIO -> World Sphere -> IO ()
generateImage 0 _ _ = do
  hPutStr stderr "\rScanlines remaining: 0 \nDone.\n"
  hFlush stderr
generateImage j g world = do
  hPutStr stderr ("\rScanlines remaining: " ++ show j ++ "\n")
  hFlush stderr
  generateLine j g world
  generateImage (j - 1) g world

someFunc :: IO ()
someFunc = do
  -- g <- MWC.createSystemRandom
  g <- MWC.create -- use for testing
  putStrLn $ printf "P3\n%d %d\n255" imageWidth imageHeight
  world <- World <$> randomScene g
  generateImage imageHeight g world

randomScene :: GenIO -> IO [Sphere]
randomScene g = do
  let groundMaterial = lambertian (color 0.5 0.5 0.5)
      largeSphere = Sphere (point 0 (-1000) 0) 1000 groundMaterial

  smallSpheres <-
    fmap catMaybes . sequence $
      ( [ do
            chooseMat <- randomDouble g
            let center = point (fromIntegral a + 0.9 * chooseMat) 0.2 (fromIntegral b + 0.9 * chooseMat)
            if Vec3.length (point 4 0.2 0 |-> center) > 0.9
              then do
                sphereMaterial <-
                  if chooseMat < 0.8
                    then do
                      albedo <- (*) <$> uniformM g <*> uniformM g
                      pure $ lambertian (Color albedo)
                    else
                      if chooseMat < 0.95
                        then do
                          albedo <- Color <$> uniformRM (0.5, 1) g
                          fuzz <- randomDoubleR 0 0.5 g
                          pure $ metal albedo fuzz
                        else do
                          pure $ dielectric 1.5
                pure $ Just (Sphere center 0.2 sphereMaterial)
              else pure Nothing
          | a <- [-11 .. 10 :: Int],
            b <- [-11 .. 10 :: Int]
        ] ::
          [IO (Maybe Sphere)]
      )

  let material1 = dielectric 1.5
      sphere1 = Sphere (point 0 1 0) 1.0 material1
  let material2 = lambertian (color 0.4 0.2 0.1)
      sphere2 = Sphere (point (-4) 1 0) 1.0 material2
  let material3 = metal (color 0.7 0.6 0.5) 0.0
      sphere3 = Sphere (point 4 1 0) 1.0 material3

  pure $ largeSphere : sphere1 : sphere2 : sphere3 : smallSpheres
