module Sampling where

import           Data.Bifunctor
import           System.Random  (RandomGen, mkStdGen, randomR)
import           Vector

sampleFloat :: RandomGen g => g -> (Float, g)
sampleFloat g = randomR (0, 1) g

sampleVector :: RandomGen g => g -> (Vec3 Float, g)
sampleVector g = (Vec3 x' y' z', g3)
  where
    (x', g1) = sampleFloat g
    (y', g2) = sampleFloat g1
    (z', g3) = sampleFloat g2

sampleUnitSphere :: RandomGen g => g -> (Vec3 Float, g)
sampleUnitSphere g = find (Vec3 1 1 1, g)
  where
    find (v, g')
      | vLengthSquared (fmap f v) < 1 = (fmap f v, g')
      | otherwise = find (sampleVector g')
      where
        f x = 2 * (x - 0.5)

sampleUnitVector :: RandomGen g => g -> (Vec3 Float, g)
sampleUnitVector g =
  let sampled = sampleUnitSphere g
   in first vUnit sampled

sampleInHemisPhere :: RandomGen g => Vec3 Float -> g -> (Vec3 Float, g)
sampleInHemisPhere normal' g
  | 0 < vDot inUnitSphere normal' = (inUnitSphere, g')
  | otherwise = (-inUnitSphere, g')
  where
    (inUnitSphere, g') = sampleUnitVector g