module Hittable.Sphere where

import           Data.Maybe

import           Hit
import           Hittable.Hittable
import           Ray
import           Vector

data Sphere =
  Sphere
    { sphereCenter   :: Point
    , sphereRadius   :: Float
    , sphereMaterial :: Material
    }

instance Hittable Sphere where
  hit sp ray hitRange
    | discriminant < 0 = Nothing
    | isNothing nearestRoot = Nothing
    | otherwise = do
      let t = fromJust nearestRoot
      let p = at ray t
      let normal = (p - sphereCenter sp) / (pure . sphereRadius) sp
      let frontFace = vDot (direction ray) normal < 0
      return $
        HitRecord
          p
          (if frontFace
             then normal
             else negate normal)
          t
          frontFace
          (sphereMaterial sp)
    where
      nearestRoot
        | isInRange rootMinus hitRange = Just rootMinus
        | isInRange rootPlus hitRange = Just rootPlus
        | otherwise = Nothing
        where
          rootMinus = ((-halfB) - sqrt discriminant) / a
          rootPlus = ((-halfB) + sqrt discriminant) / a
      (discriminant, halfB, a) = getRoot sp ray
      getRoot sp ray = (discriminant, halfB, a)
        where
          discriminant = halfB ^ 2 - a * c
          oc = origin ray - sphereCenter sp
          a = vLengthSquared . direction $ ray
          halfB = vDot oc (direction ray)
          c = vLengthSquared oc - sphereRadius sp ^ 2
