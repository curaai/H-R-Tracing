module Material.Lambertian where

import           Hit      (HitRecord (HitRecord), Scatterable (..),
                           Scattered (Scattered))
import           Ray      (Ray (Ray))
import           Sampling (sampleUnitVector)
import           Vector   (Color, vNearZero)

data Lambertian =
  Lambertian
    { albedo :: Color Float
    }

instance Scatterable Lambertian where
  scatter (Lambertian color) ray (HitRecord p normal _ _ _) g =
    (Just (Scattered (Ray p scatterDir) color), g')
    where
      (randUnitVec, g') = sampleUnitVector g
      scatterDir =
        let v = normal + randUnitVec
         in if vNearZero v
              then normal
              else v
