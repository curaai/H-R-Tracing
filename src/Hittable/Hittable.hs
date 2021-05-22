module Hittable.Hittable where

import           Ray
import           Vector

data HitRecord =
  HitRecord
    { hitPoint     :: Point
    , hitNormal    :: Vec3 Float
    , hitT         :: Float
    , hitFrontFace :: Bool
    }
  deriving (Show)

data HitRange =
  HitRange
    { hitTMin :: Float
    , hitTMax :: Float
    }
  deriving (Show, Eq)

isInRange v range = hitTMin range < v && v < hitTMax range

class Hittable a where
  hit :: a -> Ray -> HitRange -> Maybe HitRecord
