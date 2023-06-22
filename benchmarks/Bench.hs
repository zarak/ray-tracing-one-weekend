import Camera
import Criterion.Main
import MyLib
import NFUtils
import RtWeekend
import System.Random.MWC

main :: IO ()
main = do
  g <- create
  let u = fromIntegral i / fromIntegral (imageWidth - 1)
      v = fromIntegral j / fromIntegral (imageHeight - 1)
      i = imageWidth `div` 2
      j = imageHeight - 20
      r = getRay u v
  defaultMain
    [ bench ("generateLine " <> show j) $ nfIO $ (`generateLine` g) j,
      bench "rayColor" $ nfIO $ rayColor r g maximumDepth
    ]
