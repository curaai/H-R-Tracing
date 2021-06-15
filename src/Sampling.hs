module Sampling where

import           Data.Bifunctor (Bifunctor (first))
import           Data.Bool      (bool)
import           System.Random  (RandomGen, mkStdGen, randomR)
import           Vector         (Vec3 (Vec3), vDot, vLengthSquared, vUnit)

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
      | vLengthSquared v' < 1 = (v', g')
      | otherwise = find (sampleVector g')
      where
        v' = fmap ((* 2) . (+ (-0.5))) v

sampleUnitVector :: RandomGen g => g -> (Vec, g)
sampleUnitVector g =
  let sampled = sampleUnitSphere g
   in first vUnit sampled

sampleInHemisPhere :: RandomGen g => Vec -> g -> (Vec, g)
sampleInHemisPhere normal' g =
  (bool negate id (0 < vDot inUnitSphere normal') inUnitSphere, g')
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
