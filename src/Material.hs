module Material where

import Color
import Control.Monad.Primitive
import MaterialHit
import Ray
import RtWeekend
import System.Random.MWC (Gen)
import Vec3

lambertian :: PrimMonad m => Color -> Gen (PrimState m) -> m Material
lambertian albedo g = do
  randomVec <- unitVector <$> randomInUnitSphere g
  let a = albedo
      f :: Ray -> HitRecord -> Maybe Scattered
      f _ hitRecord = do
        let scatterDirection' = hitRecord.normal + randomVec
            scatterDirection =
              if nearZero scatterDirection'
                then hitRecord.normal
                else scatterDirection
            scattered = Ray hitRecord.p scatterDirection
        pure $ Scattered scattered albedo
  pure $ Material f a
