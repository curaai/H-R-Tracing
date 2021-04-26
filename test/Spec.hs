import           Data.List
import           Data.Ord

import           Test.Tasty
import           Test.Tasty.HUnit

import           Img
import           Ray
import           Vector

import           TestVec

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, testRay, testVectors]

testRay =
  testGroup
    "Test Ray operations"
    [testCase "At operation" $ at r 1.2 @?= Vec3 0 0 1.2]
  where
    r = Ray (Vec3 0 0 0) (Vec3 0 0 1)

unitTests =
  testGroup
    "Unit tests"
    [testCase "Gradient Image" $ (calcSize . gradientImg) oriSize @?= oriSize]
  where
    oriSize = Size 8 10
    calcSize arr = Size (length . head $ arr) (length arr)
