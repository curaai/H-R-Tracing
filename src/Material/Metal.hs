module Material.Metal where

import           Hit      (HitRecord (HitRecord), Scatterable (..),
                           Scattered (Scattered))
import           Ray      (Ray (Ray, direction))
import           Sampling (sampleUnitSphere)
import           Vector   (Color, vDot, vReflect, vUnit)

data Metal =
  Metal
    { albedo :: Color Float
    , fuzz   :: Float
    }

instance Scatterable Metal where
  scatter (Metal color f) ray (HitRecord p normal _ _ _) g
    | 0 < vDot (direction scatterRay) normal =
      (Just (Scattered scatterRay color), g)
    | otherwise = (Nothing, g)
    where
      (randUnitSphere, g') = sampleUnitSphere g
      reflected = vReflect (vUnit . direction $ ray) normal
      scatterRay = Ray p (reflected + (pure . min 1 $ f) * randUnitSphere)
