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
