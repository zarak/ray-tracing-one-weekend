{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fold" #-}
module MyLib (someFunc, rayColor, generateLine) where

import Camera
import Color (Color (..), color, scaleColor, white, writeColor)
import Control.Monad (forM, replicateM)
import Control.Monad.Primitive
import Control.Parallel.Strategies (parBuffer, rdeepseq, withStrategy)
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
imageWidth = 40

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

rayColor :: (PrimMonad m) => Ray -> Gen (PrimState m) -> Int -> m (World Sphere) -> m Color
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

drawRay :: PrimMonad m => Int -> Int -> Gen (PrimState m) -> m (World Sphere) -> m Color
drawRay i j g world = do
  x <- randomDouble g
  y <- randomDouble g
  randomInDisk <- randomInUnitDisk g
  let u = (fromIntegral i + x) / fromIntegral (imageWidth - 1)
      v = (fromIntegral j + y) / fromIntegral (imageHeight - 1)
      r = getRay camera u v randomInDisk
      pixelColor = rayColor r g maximumDepth world
   in pixelColor

generateLine :: Int -> Gen (PrimState IO) -> IO (World Sphere) -> IO ()
generateLine j g world = do
  sampledColors <- forM [1 .. imageWidth] $ \i -> do
    cs <- replicateM samplesPerPixel $ drawRay i j g world
    let summedColors = foldr (<>) mempty cs
    pure $ writeColor summedColors samplesPerPixel
  T.putStrLn $ T.unlines $ withStrategy (parBuffer 100 rdeepseq) sampledColors

generateImage :: Int -> Gen (PrimState IO) -> IO (World Sphere) -> IO ()
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
  let -- placeholder

  world <- mkWorld <$> randomScene g
  generateImage imageHeight g world

mkWorld :: (PrimMonad m, Hittable a) => [m a] -> m (World a)
mkWorld xs = World <$> sequence xs

randomScene :: Gen (PrimState IO) -> IO [IO Sphere]
randomScene g = do
  let groundMaterial = lambertian (color 0.5 0.5 0.5) g
      largeSphere = Sphere (point 0 (-1000) 0) 1000 <$> groundMaterial :: IO Sphere

  smallSpheres <-
    fmap catMaybes . sequence $
      [ do
          chooseMat <- randomDouble g
          let center = point (fromIntegral a + 0.9 * chooseMat) 0.2 (fromIntegral b + 0.9 * chooseMat)
          if Vec3.length (point 4 0.2 0 |-> center) > 0.9
            then do
              sphereMaterial <-
                if chooseMat < 0.8
                  then do
                    albedo <- (*) <$> uniformM g <*> uniformM g
                    lambertian (Color albedo) g
                  else
                    if chooseMat < 0.95
                      then do
                        albedo <- Color <$> uniformRM (0.5, 1) g
                        fuzz <- randomDoubleR 0 0.5 g
                        metal albedo fuzz g
                      else do
                        dielectric 1.5 g
              pure $ Just $ pure (Sphere center 0.2 sphereMaterial)
            else pure Nothing
        | a <- [-11 .. 10 :: Int],
          b <- [-11 .. 10 :: Int]
      ]

  let material1 = dielectric 1.5 g
      sphere1 = Sphere (point 0 1 0) 1.0 <$> material1 :: IO Sphere
  let material2 = lambertian (color 0.4 0.2 0.1) g
      sphere2 = Sphere (point (-4) 1 0) 1.0 <$> material2 :: IO Sphere
  let material3 = metal (color 0.7 0.6 0.5) 0.0 g
      sphere3 = Sphere (point 4 1 0) 1.0 <$> material3 :: IO Sphere

  pure $ largeSphere : sphere1 : sphere2 : sphere3 : smallSpheres
