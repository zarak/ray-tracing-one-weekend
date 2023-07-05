module Material where

import Color
import MaterialHit
import Ray
import RtWeekend
import System.Random.MWC (GenIO)
import Vec3

lambertian :: Color -> Material
lambertian albedo =
  let f :: GenIO -> Ray -> HitRecord -> IO (Maybe Scattered)
      f g rayIn hitRecord = do
        randomVec <- unitVector <$> randomInUnitSphere g
        let scatterDirection =
              if nearZero $ hitRecord.normal + randomVec
                then hitRecord.normal
                else hitRecord.normal + randomVec
            scattered = Ray hitRecord.p scatterDirection rayIn.time
        pure $ Just $ Scattered scattered albedo
   in Material f

metal :: Color -> Double -> Material
metal albedo fuzz =
  let f :: GenIO -> Ray -> HitRecord -> IO (Maybe Scattered)
      f g rayIn hitRecord = do
        randomVec <- randomInUnitSphere g
        let reflected = reflect (unitVector rayIn.direction) hitRecord.normal
            scattered = Ray hitRecord.p (reflected + fuzz *^ randomVec) rayIn.time
        if dot scattered.direction hitRecord.normal > 0
          then pure $ Just $ Scattered scattered albedo
          else pure Nothing
   in Material f

dielectric :: Double -> Material
dielectric ir =
  let f :: GenIO -> Ray -> HitRecord -> IO (Maybe Scattered)
      f g rayIn rec = do
        rd <- randomDouble g
        let attenuation = white
            refractionRatio =
              case rec.face of
                Front -> 1.0 / ir
                Back -> ir
            unitDirection = unitVector rayIn.direction
            cosTheta = min (dot (-unitDirection) rec.normal) 1.0
            sinTheta = sqrt (1.0 - cosTheta * cosTheta)
            cannotRefract = refractionRatio * sinTheta > 1.0
            direction
              | cannotRefract
                  || reflectance cosTheta refractionRatio > rd =
                  reflect unitDirection rec.normal
              | otherwise = refract unitDirection rec.normal refractionRatio
            scattered = Ray rec.p direction rayIn.time
        pure $ Just $ Scattered scattered attenuation
   in Material f

reflectance :: Double -> Double -> Double
reflectance cosine refIdx =
  let frac = (1 - refIdx) / (1 + refIdx)
      r0 = frac * frac
   in r0 + (1 - r0) * (1 - cosine) ** 5
