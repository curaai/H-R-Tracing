module Material.Lambertian where

import           Hit
import           Ray
import           Sampling
import           Vector

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
