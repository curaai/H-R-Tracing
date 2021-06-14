{-# LANGUAGE ExistentialQuantification #-}

module Hit where

import           Ray
import           Sampling
import           System.Random
import           Vector

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
