module Sampling where

import           Data.Bifunctor (Bifunctor (first))
import           System.Random  (RandomGen, mkStdGen, randomR)
import           Vector         (Vec3 (Vec3), vDot, vLengthSquared, vUnit, Vec)

sampleFloat :: RandomGen g => g -> (Float, g)
sampleFloat g = randomR (0, 1) g

sampleVector :: RandomGen g => g -> (Vec, g)
sampleVector g = (Vec3 x' y' z', g3)
  where
    (x', g1) = sampleFloat g
    (y', g2) = sampleFloat g1
    (z', g3) = sampleFloat g2

sampleVectorR :: RandomGen b => b -> Float -> Float -> (Vec, b)
sampleVectorR g min' max' = (Vec3 x' y' z', g3)
  where
    sampleFloat' g = randomR (min', max') g
    (x', g1) = sampleFloat' g
    (y', g2) = sampleFloat' g1
    (z', g3) = sampleFloat' g2

sampleUnitSphere :: RandomGen g => g -> (Vec, g)
sampleUnitSphere g = find (Vec3 1 1 1, g)
  where
    find (v, g')
      | vLengthSquared (fmap f v) < 1 = (fmap f v, g')
      | otherwise = find (sampleVector g')
      where
        f x = 2 * (x - 0.5)

sampleUnitVector :: RandomGen g => g -> (Vec, g)
sampleUnitVector g =
  let sampled = sampleUnitSphere g
   in first vUnit sampled

sampleInHemisPhere :: RandomGen g => Vec -> g -> (Vec, g)
sampleInHemisPhere normal' g
  | 0 < vDot inUnitSphere normal' = (inUnitSphere, g')
  | otherwise = (-inUnitSphere, g')
  where
    (inUnitSphere, g') = sampleUnitVector g

sampleInUnitDisk :: RandomGen g => g -> (Vec, g)
sampleInUnitDisk g = find (Vec3 1 1 1, g)
  where
    find (v, g')
      | vLengthSquared v < 1 = (v, g2)
      | otherwise = find (Vec3 x' y' 0, g2)
      where
        sampleFloat' g = randomR (-1, 1) g
        (x', g1) = sampleFloat' g'
        (y', g2) = sampleFloat' g1
