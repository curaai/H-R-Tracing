module TestRay
  ( testRay
  ) where

import           Ray
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC
import           Vector

testRay = testGroup "Test Ray Functions" [t0, t1]

t0 = testCase "At" $ at r 1.2 @?= Vec3 0 0 1.2
  where
    r = Ray (Vec3 0 0 0) (Vec3 0 0 1)

t1 = QC.testProperty "At with quickcheck" $ \x -> at r x == Vec3 x x x
  where
    r = Ray (Vec3 0 0 0) (Vec3 1 1 1)
