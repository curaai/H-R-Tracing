{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Hittable.Hittable where

import           Ray
import           Vector

import           Data.Maybe

data HitRecord =
  HitRecord
    { point     :: Point
    , normal    :: Vec
    , root      :: Float
    , frontface :: Bool
    }
  deriving (Show)

faceNormal :: HitRecord -> Ray -> Vec -> HitRecord
faceNormal record ray outnormal =
  HitRecord (point record) normal' (root record) frontface'
  where
    frontface' = vDot (direction ray) outnormal < 0
    normal' =
      if frontface'
        then outnormal
        else (-outnormal)

data RootRange =
  RootRange
    { tMin :: Float
    , tMax :: Float
    }
  deriving (Show)

findNearestRoot :: RootRange -> (Float, Float) -> Maybe Float
findNearestRoot rr roots
  | inRange lRoot = Just lRoot
  | inRange rRoot = Just rRoot
  | otherwise = Nothing
  where
    lRoot = fst roots
    rRoot = snd roots
    inRange x = tMin rr < x && x < tMax rr

class Hittable a where
  shotRay :: a -> Ray -> RootRange -> Maybe HitRecord
