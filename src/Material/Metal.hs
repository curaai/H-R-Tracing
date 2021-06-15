module Material.Metal where

import           Hit      (HitRecord (HitRecord, hitNormal, hitPoint),
                           Scatterable (..),
                           Scattered (Scattered, scatteredRay))

import           Ray      (Ray (Ray, direction))
import           Sampling (sampleUnitSphere)
import           Vector   (Color, vDot, vReflect, vUnit)

data Metal =
  Metal
    { albedo :: Color Float
    , fuzz   :: Float
    }

whenMaybe :: Bool -> a -> Maybe a
whenMaybe False _ = Nothing
whenMaybe True a  = Just a

instance Scatterable Metal where
  scatter (Metal color f) (Ray _ dir) HitRecord { hitPoint = p
                                                , hitNormal = normal
                                                } g =
    (whenMaybe (0 < vDot (direction sctRay) normal) res, g')
    where
      (randUnitSphere, g') = sampleUnitSphere g
      sctRay =
        let reflected = vReflect (vUnit dir) normal
         in Ray p (reflected + (pure . min 1 $ f) * randUnitSphere)
      res = Scattered sctRay color
