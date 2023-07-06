{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fold" #-}
{-# HLINT ignore "Redundant pure" #-}
module MyLib (someFunc, rayColor, generateLine, randomScene) where

import Camera
import Color (Color (..), color, scaleColor, white, writeColor)
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (forM, replicateM)
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Data.Text.IO qualified as T (putStrLn)
import GHC.Real (infinity)
import Hittable
import Material
import MaterialHit
import MovingSphere (MovingSphere (..))
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
imageWidth = 400

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
    time0
    time1
  where
    lookfrom = point 13 2 3
    lookat = Point zeros
    vup = Vec3 0 1 0
    distToFocus = 10.0
    aperture = 0.1
    time0 = 0.0
    time1 = 1.0

rayColor :: Ray -> GenIO -> Int -> World AnyHittable -> IO Color
rayColor _ _ 0 _ = pure mempty
rayColor r g depth world = do
  let unitDirection = unitVector r.direction
      t = 0.5 * (unitDirection.y + 1.0)
      blue = color 0.5 0.7 1.0
      blueWhiteLerp = scaleColor (1.0 - t) white <> scaleColor t blue

  case hit world r (shadowAcne, fromRational infinity) of
    Nothing -> pure blueWhiteLerp
    Just rec -> do
      mScattered <- rec.material.scatter g r rec
      case mScattered of
        Nothing -> pure mempty
        Just scattered -> do
          c <- rayColor scattered.ray g (depth - 1) world
          pure $ Color $ scattered.attenuation.toVec3 * c.toVec3

drawRay :: Int -> Int -> World AnyHittable -> IO Color
drawRay i j world = do
  g <- MWC.create
  x <- randomDouble g
  y <- randomDouble g
  let u = (fromIntegral i + x) / fromIntegral (imageWidth - 1)
      v = (fromIntegral j + y) / fromIntegral (imageHeight - 1)
  r <- cameraRay camera u v g
  pixelColor <- rayColor r g maximumDepth world
  pure pixelColor

generateLine :: Int -> World AnyHittable -> IO ()
generateLine j world = do
  sampledColors <-
    mapConcurrently
      ( \i -> do
          cs <- replicateM samplesPerPixel $ drawRay i j world
          let summedColors = foldr (<>) mempty cs
          pure $ writeColor summedColors samplesPerPixel
      )
      [1 .. imageWidth]
  T.putStrLn $ T.unlines sampledColors

generateImage :: Int -> World AnyHittable -> IO ()
generateImage 0 _ = do
  hPutStr stderr "\rScanlines remaining: 0 \nDone.\n"
  hFlush stderr
generateImage j world = do
  hPutStr stderr ("\rScanlines remaining: " ++ show j ++ "\n")
  hFlush stderr
  generateLine j world
  generateImage (j - 1) world

someFunc :: IO ()
someFunc = do
  -- g <- MWC.createSystemRandom
  g <- MWC.create -- use for testing
  putStrLn $ printf "P3\n%d %d\n255" imageWidth imageHeight
  world <- World <$> randomScene g
  generateImage imageHeight world

randomScene :: GenIO -> IO [AnyHittable]
randomScene g = do
  let groundMaterial = lambertian (color 0.5 0.5 0.5)
      largeSphere = AnyHittable (Sphere (point 0 (-1000) 0) 1000 groundMaterial)

  smallSpheres <-
    fmap catMaybes . sequence $
      ( [ do
            chooseMat <- randomDouble g
            let center = point (fromIntegral a + 0.9 * chooseMat) 0.2 (fromIntegral b + 0.9 * chooseMat)
            if Vec3.length (point 4 0.2 0 |-> center) > 0.9
              then do
                if chooseMat < 0.8
                  then do
                    albedo <- (*) <$> uniformM g <*> uniformM g
                    let material = lambertian (Color albedo)
                    x <- randomDoubleR 0 0.5 g
                    let center2 = Point (Vec3 0 x 0 + center.toVec3)
                    pure $ Just (AnyHittable $ MovingSphere center center2 0.0 1.0 0.2 material)
                  else
                    if chooseMat < 0.95
                      then do
                        albedo <- Color <$> uniformRM (0.5, 1) g
                        fuzz <- randomDoubleR 0 0.5 g
                        pure $ Just (AnyHittable $ Sphere center 0.2 (metal albedo fuzz))
                      else do
                        pure $ Just (AnyHittable $ Sphere center 0.2 (dielectric 1.5))
              else pure Nothing
          | a <- [-11 .. 10 :: Int],
            b <- [-11 .. 10 :: Int]
        ]
      )

  let material1 = dielectric 1.5
      sphere1 = AnyHittable $ Sphere (point 0 1 0) 1.0 material1
  let material2 = lambertian (color 0.4 0.2 0.1)
      sphere2 = AnyHittable $ Sphere (point (-4) 1 0) 1.0 material2
  let material3 = metal (color 0.7 0.6 0.5) 0.0
      sphere3 = AnyHittable $ Sphere (point 4 1 0) 1.0 material3

  pure $ largeSphere : sphere1 : sphere2 : sphere3 : smallSpheres
