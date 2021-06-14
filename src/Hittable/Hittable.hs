module Hittable.Hittable where

import           Hit (HitRange (hitTMax, hitTMin), HitRecord)
import           Ray (Ray)

isInRange :: Float -> HitRange -> Bool
isInRange v range = hitTMin range < v && v < hitTMax range

class Hittable a where
  hit :: a -> Ray -> HitRange -> Maybe HitRecord
