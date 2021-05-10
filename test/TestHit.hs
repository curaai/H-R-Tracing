module TestHit where

import           Camera
import           Data.Either
import           Data.Maybe            (isJust)
import           Hittable.Sphere
import           Ray
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC
import           Vector

testSphere = testGroup "Test Hit to sphere" [t0, t1]

t0 = testCase "Single sphere case" $ isJust (hitRoot s r) @?= True
  where
    r = Ray (Vec3 0 0 0) (Vec3 0 0 1)
    s = Sphere (Vec3 0 0 0) 0.5

t1 = testCase "Two sphere case" $ isLeft (hitRay [s1, s2] r) @?= True
  where
    r = Ray (Vec3 0 0 0) (Vec3 0 0 (-1))
    s1 = Sphere (Vec3 0 0 (-1)) 0.5
    s2 = Sphere (Vec3 0 (-100.5) (-1)) 100
