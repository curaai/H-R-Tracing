module Material.Metal where

import           Hit
import           Ray
import           Sampling
import           Vector

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
