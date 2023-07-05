import Camera
import Color (Color)
import Criterion.Main
import MyLib
import NFUtils
import Ray (Ray (..))
import RtWeekend
import Sphere
import System.Random.MWC
import Vec3

imageWidth :: Int
imageWidth = 200

imageHeight :: Int
imageHeight = truncate $ fromIntegral imageWidth / aspectRatio

aspectRatio :: Double
aspectRatio = 16.0 / 9.0

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

main :: IO ()
main = do
  g <- create
  randomInDisk <- randomInUnitDisk g
  world <- World <$> randomScene g
  let u = fromIntegral i / fromIntegral (imageWidth - 1)
      v = fromIntegral j / fromIntegral (imageHeight - 1)
      i = imageWidth `div` 2
      j = imageHeight - 20
      r = cameraRay camera u v randomInDisk
  defaultMain
    [ -- bench ("generateLine " <> show j) $ nfIO $ (`generateLine` g) j,
      bench "rayColor" $ nfIO $ rayColor r g maximumDepth world
    ]
