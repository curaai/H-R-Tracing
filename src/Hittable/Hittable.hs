{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Hittable.Hittable where

import           Ray
import           Vector

import           Data.Maybe

data HitRecord =
  HitRecord
    { point     :: Point
    , normal    :: Vec
    , tangent   :: Float
    , frontface :: Bool
    }
  deriving (Show)

faceNormal :: HitRecord -> Ray -> Vec -> HitRecord
faceNormal record ray outnormal =
  HitRecord (point record) normal' (tangent record) frontface'
  where
    frontface' = vDot (direction ray) outnormal < 0
    normal' =
      if frontface'
        then outnormal
        else (-outnormal)

type TangentRange = (Float, Float)

class Hittable a where
  hit :: a -> Ray -> TangentRange -> HitRecord -> Maybe HitRecord

