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

hit s r
  | t < 0 = Nothing
  | otherwise = Just ((n + 1) *: 0.5)
  where
    n = vUnit (at r t - Vec3 0 0 (-1))
    t =
      if res > 0
        then ((-b) - sqrt res) / (2 * a)
        else -1
      where
        res = b * b - 4 * a * c
        oc = origin r - center s
        a =
          let dir = direction r
           in vDot dir dir
        b = 2 * vDot oc (direction r)
        c =
          vDot oc oc -
          let radius' = radius s
           in radius' * radius'
