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
  let f :: Ray -> HitRecord -> Maybe Scattered
      f _ hitRecord = do
        let scatterDirection' = hitRecord.normal + randomVec
            scatterDirection =
              if nearZero scatterDirection'
                then hitRecord.normal
                else scatterDirection
            scattered = Ray hitRecord.p scatterDirection
        pure $ Scattered scattered albedo
  pure $ Material f albedo

metal :: Color -> Material
metal albedo = do
  let f :: Ray -> HitRecord -> Maybe Scattered
      f rayIn hitRecord = do
        let reflected = reflect (unitVector rayIn.direction) hitRecord.normal
            scattered = Ray hitRecord.p reflected
        if dot scattered.direction hitRecord.normal > 0
          then Just $ Scattered scattered albedo
          else Nothing
  Material f albedo
