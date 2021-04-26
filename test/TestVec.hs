module TestVec
  ( testVectors
  ) where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Vector

truncate' :: Double -> Double
truncate' x = fromIntegral (floor (x * t)) / t
  where
    t = 10 ^ 4

testVectors = testGroup "Test Vector Operations" [t0, t1, t2, t3]

t0 = testCase "Dot Product" $ vDot v1 v2 @?= 0
  where
    v1 = Vec3 6 (-1) 3
    v2 = Vec3 4 18 (-2)

t1 = testCase "Cross Product" $ vCross v1 v2 @?= Vec3 0 0 0
  where
    v1 = Vec3 3 (-3) 1
    v2 = Vec3 (-12) 12 (-4)

t2 =
  testCase "Unit Vector" $
  fmap truncate' (vUnit v1) @?=
  fmap truncate' (Vec3 (sqrt $ 2 / 3) (1 / sqrt 6) (1 / sqrt 6))
  where
    v1 = Vec3 2 1 1

t3 = testCase "Vector Magnitude" $ vLength v1 @?= 3
  where
    v1 = Vec3 1 2 2
