module Hitable where

import           Ray
import           Vector

import           Data.Maybe

-- class Hitable a where
--     hit :: Floating b => a -> Ray -> Maybe (Vec3 b)

data Sphere =
  Sphere
    { center :: Point
    , radius :: Float
    }

hit :: Sphere -> Ray -> Maybe (Vec3 Float)
hit s r
  | t < 0 = Nothing
  | otherwise = Just ((n + 1) *: 0.5)
  where
    n = vUnit (at r t - Vec3 0 0 (-1))
    t =
      if discriminant > 0
        then ((-halfB) - sqrt discriminant) / a
        else -1
      where
        discriminant = halfB^2 - a*c
        oc = origin r - center s
        a = vLengthSqured . direction $ r
        halfB = vDot oc (direction r)
        c = vLengthSqured oc - radius s ^ 2
  
