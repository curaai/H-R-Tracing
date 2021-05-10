{-# LANGUAGE FlexibleInstances #-}

module Hittable.Sphere where

import           System.Random

import           Hittable.Hittable

import           Ray
import           Vector

import           Control.Monad
import           Data.Either
import           Data.List.Split
import           Data.Maybe

data Sphere =
  Sphere
    { center :: Point
    , radius :: Float
    }

instance Hittable Sphere where
  shotRay sp ray rr
    | isNothing normal_ = Nothing
    | isNothing root' = Nothing
    | otherwise = Just (faceNormal hr' ray outnormal')
    where
      normal_ = hitRoot sp ray
      root' = findNearestRoot rr $ fromJust normal_
      point' = at ray $ fromJust root'
      outnormal' = (point' - center sp) *: (1 / radius sp)
      hr' = HitRecord point' vec (fromJust root') True

instance (Hittable a) => Hittable [a] where
  shotRay a ray rr = foldl f Nothing a
    where
      f :: Hittable a => Maybe HitRecord -> a -> Maybe HitRecord
      f hr obj =
        let res = shotRay obj ray (rr' hr)
         in if isNothing res
              then hr
              else res
        where
          rr' hr = RootRange 0 (maybe 9999 root hr)

{-|
discriminant = h^2 - ac
d > 0 => 2 solution
d == 0 ==> 1 solution
d < 0 => no solution
-}
hitRoot :: Sphere -> Ray -> Maybe (Float, Float)
hitRoot sphere ray
  | discriminant < 0 = Nothing
  | otherwise =
    Just
      (((-halfB) - sqrt discriminant) / a, ((-halfB) + sqrt discriminant) / a)
  where
    discriminant = halfB ^ 2 - a * c
    oc = origin ray - center sphere
    a = vLengthSquared . direction $ ray
    halfB = vDot oc (direction ray)
    c = vLengthSquared oc - radius sphere ^ 2
