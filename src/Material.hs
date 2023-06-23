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
        let scatterDirection =
              if nearZero $ hitRecord.normal + randomVec
                then hitRecord.normal
                else hitRecord.normal + randomVec
            scattered = Ray hitRecord.p scatterDirection
        pure $ Scattered scattered albedo
  pure $ Material f

metal :: PrimMonad m => Color -> Double -> Gen (PrimState m) -> m Material
metal albedo fuzz g = do
  randomVec <- randomInUnitSphere g
  let f :: Ray -> HitRecord -> Maybe Scattered
      f rayIn hitRecord = do
        let reflected = reflect (unitVector rayIn.direction) hitRecord.normal
            scattered = Ray hitRecord.p (reflected + fuzz *^ randomVec)
        if dot scattered.direction hitRecord.normal > 0
          then Just $ Scattered scattered albedo
          else Nothing
  pure $ Material f

dielectric :: PrimMonad m => Double -> Gen (PrimState m) -> m Material
dielectric ir _ = do
  let f :: Ray -> HitRecord -> Maybe Scattered
      f rayIn hitRecord = do
        let attenuation = color 1.0 1.0 1.0
            refractionRatio =
              if hitRecord.face == Front
                then 1.0 / ir
                else ir
            unitDirection = unitVector rayIn.direction
            refracted = refract unitDirection hitRecord.normal refractionRatio
            scattered = Ray hitRecord.p refracted
        pure $ Scattered scattered attenuation
  pure $ Material f
