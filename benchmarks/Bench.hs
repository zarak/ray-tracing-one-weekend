import Camera
import Criterion.Main
import MyLib
import NFUtils
import RtWeekend
import System.Random.MWC

rayColor' :: PrimMonad m => Ray -> Gen (PrimState m) -> Int -> m Color
rayColor' r g depth = do
  let unitDirection = unitVector r . direction
      t = 0.5 * (unitDirection . y + 1.0)
      blue = color 0.5 0.7 1.0
      blueWhiteLerp = scaleColor (1.0 - t) white <> scaleColor t blue
  case hit world r (shadowAcne, fromRational infinity) of
    Nothing -> pure blueWhiteLerp
    Just rec -> do
      -- Use normalized unitVector in this version
      randomUnitVec <- unitVector <$> randomInUnitSphere g
      let target = rec . p . toVec3 + rec . normal + randomUnitVec
          bounce = Ray rec . p (rec . p |-> Point target)
      newColor <- rayColor bounce g (depth - 1)
      pure $ scaleColor 0.5 newColor

main :: IO ()
main = do
  g <- create
  let u = fromIntegral i / fromIntegral (imageWidth - 1)
      v = fromIntegral j / fromIntegral (imageHeight - 1)
      i = imageWidth `div` 2
      j = imageHeight - 20
      r = getRay u v
  defaultMain
    [ -- bench ("generateLine " <> show j) $ nfIO $ (`generateLine` g) j,
      bench "rayColor" $ nfIO $ rayColor r g maximumDepth,
      bench "rayColor'" $ nfIO $ rayColor' r g maximumDepth
    ]
