import           Data.List
import           Data.Ord

import           Test.Tasty
import           Test.Tasty.HUnit

import           Camera
import           Img
import           Ray
import           Vector

import           TestCamera
import           TestHit
import           TestRay
import           TestVec

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [testRay, testVectors, testSphere, testCamera]
