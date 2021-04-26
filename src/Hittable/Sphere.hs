module Hittable.Sphere where

import           Hittable.Hittable

import           Ray
import           Vector

import           Data.Maybe

data Sphere =
  Sphere
    { center :: Point
    , radius :: Float
    }

instance Hittable Sphere where
  hit sp ray tr hr
    | discriminant < 0 || isNothing nearestRoot = Nothing
    | otherwise = Just (faceNormal hr' ray outnormal')
    where
      point' = at ray $tangent hr
      tangent' = fromJust nearestRoot
      outnormal' = (point' - center sp) *: (1 / radius sp)
      hr' = HitRecord point' vec tangent' True
      nearestRoot =
        if checkRange rootA
          then if checkRange rootB
                 then Nothing
                 else Just rootB
          else Just rootA
        where
          rootA = ((-halfB) - sqrtd) / a
          rootB = ((-halfB) + sqrtd) / a
          checkRange root = root < fst tr || snd tr < root
      
      sqrtd = sqrt discriminant
      discriminant = halfB ^ 2 - a * c
      oc = origin ray - center sp
      a = vLengthSqured . direction $ ray
      halfB = vDot oc (direction ray)
      c = vLengthSqured oc - radius sp ^ 2
