module Material.Lambertian where

import           Data.Bool (bool)
import           Hit       (HitRecord (HitRecord, hitNormal, hitPoint),
                            Scatterable (..), Scattered (Scattered))

import           Ray       (Ray (Ray))
import           Sampling  (sampleUnitVector)
import           Vector    (Color, vNearZero)

newtype Lambertian =
  Lambertian
    { albedo :: Color Float
    }

instance Scatterable Lambertian where
  scatter (Lambertian c) _ HitRecord {hitPoint = p, hitNormal = n} g =
    (Just (Scattered (Ray p scatterDir) c), g')
    where
      (randUnitVec, g') = sampleUnitVector g
      scatterDir =
        let v = n + randUnitVec
         in bool v n (vNearZero v)
