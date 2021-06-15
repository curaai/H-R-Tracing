module Hittable.Hittable where

import           Hit
import           Ray (Ray)

isInRange :: Float -> HitRange -> Bool
isInRange v (HitRange tmin tmax) = tmin < v && v < tmax

class Hittable a where
  hit :: a -> Ray -> HitRange -> Maybe HitRecord
