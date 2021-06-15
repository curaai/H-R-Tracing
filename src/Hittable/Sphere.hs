module Hittable.Sphere
  ( Sphere(Sphere)
  ) where

import           Data.Bool         (bool)
import           Data.Maybe        (fromJust, isNothing)

import           Hit               (HitRange, HitRecord (HitRecord), Material)
import           Hittable.Hittable (Hittable (..), isInRange)
import           Ray               (Ray (Ray, direction), at)
import           Vector            (Point, vDot, vLengthSquared)

data Sphere =
  Sphere
    { sphereCenter   :: Point
    , sphereRadius   :: Float
    , sphereMaterial :: Material
    }

instance Hittable Sphere where
  hit sp@(Sphere ctr radius mtr) ray hitRange = do
    t <- findNearestRoot sp ray hitRange
    let p = at ray t
        normal = (p - ctr) / pure radius
        frontFace = 0 > vDot (direction ray) normal
    return $ HitRecord p (bool negate id frontFace normal) t frontFace mtr

findNearestRoot :: Sphere -> Ray -> HitRange -> Maybe Float
findNearestRoot sphere ray hitRange
  | discriminant < 0 = Nothing
  | otherwise = nearestRoot
  where
    (discriminant, halfB, a) = findRoot sphere ray
    nearestRoot
      | isInRange rootMinus hitRange = Just rootMinus
      | isInRange rootPlus hitRange = Just rootPlus
      | otherwise = Nothing
      where
        rootMinus = ((-halfB) - sqrt discriminant) / a
        rootPlus = ((-halfB) + sqrt discriminant) / a
    findRoot (Sphere ctr radius _) (Ray ori dir) = (discriminant, halfB, a)
      where
        discriminant = halfB * halfB - a * c
        a = vLengthSquared dir
        c = vLengthSquared oc - radius * radius
        oc = ori - ctr
        halfB = vDot oc dir
