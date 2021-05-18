module TestCam where

import           Camera
import           Ray
import           Vector

import           Test.Tasty
import           Test.Tasty.HUnit

testRender = testGroup "Test Hit and rendering" [t0]

t0 = testCase "Ray2Color" $ ray2color r @?= Vec3 1 0 0
  where
    r = Ray (Vec3 0 0 0) (Vec3 0 0 (-1))
