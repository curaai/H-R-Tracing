module Material.Dielectric where

import           Data.Bool (bool)
import           Hit       (HitRecord (HitRecord), Scatterable (..),
                            Scattered (Scattered))
import           Ray       (Ray (Ray, direction))
import           Sampling  (sampleFloat)
import           Vector    (Vec3 (Vec3), vDot, vReflect, vRefract, vUnit)

newtype Dielectric =
  Dielectric
    { ir :: Float
    }

instance Scatterable Dielectric where
  scatter (Dielectric ir') ray (HitRecord p normal _ frontFace _) g =
    (Just (Scattered scattered attenuation), g')
    where
      (randFloat, g') = sampleFloat g
      scattered = Ray p refracted
      attenuation = Vec3 1 1 1
      refracted =
        if (let cosTheta = min (vDot (-unitDir) normal) 1
                sinTheta = sqrt $ 1 - cosTheta ^ 2
             in refractionRatio * sinTheta > 1 ||
                reflectance cosTheta refractionRatio > randFloat)
          then vReflect unitDir normal
          else vRefract unitDir normal refractionRatio
        where
          unitDir = vUnit . direction $ ray
          refractionRatio = bool id recip frontFace ir'
          reflectance cosine refIdx =
            let r = (^ 2) $ (1 - refIdx) / (1 + refIdx)
             in r + (1 - r) * (1 - cosine) ^ 5
