{-# LANGUAGE ExistentialQuantification #-}

module Hit where

import           Ray           (Ray)
import           System.Random (RandomGen)
import           Vector        (Point, Vec)

data HitRecord =
  HitRecord
    { hitPoint     :: !Point
    , hitNormal    :: !Vec
    , hitT         :: !Float
    , hitFrontFace :: !Bool
    , hitMaterial  :: !Material
    }

data HitRange =
  HitRange
    { hitTMin :: !Float
    , hitTMax :: !Float
    }
  deriving (Show, Eq)

data Material =
  forall a. Scatterable a =>
            Material a

data Scattered =
  Scattered
    { scatteredRay     :: !Ray
    , attenuationColor :: !Vec
    }

class Scatterable a where
  scatter :: (RandomGen g) => a -> Ray -> HitRecord -> g -> (Maybe Scattered, g)
