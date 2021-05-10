module Sampling where

import           System.Random (RandomGen, mkStdGen, randomR)
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
