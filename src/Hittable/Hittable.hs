module Hittable.Hittable where

import           Hit
import           Ray
import           Vector

isInRange v range = hitTMin range < v && v < hitTMax range

class Hittable a where
  hit :: a -> Ray -> HitRange -> Maybe HitRecord
