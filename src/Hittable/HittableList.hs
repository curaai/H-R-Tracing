module Hittable.HittableList where

import           Hittable.Hittable

import           Ray
import           Vector

import           Data.Maybe

import           Hittable.Sphere

-- instance Hittable [Hittable] where
  -- hit :: [a] -> Ray -> TangentRange -> HitRecord -> Maybe HitRecord
  -- hit hList ray tr hr = Nohting
