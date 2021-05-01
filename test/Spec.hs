import           Data.List
import           Data.Ord

import           Test.Tasty
import           Test.Tasty.HUnit

import           Img
import           Ray
import           Vector

import           TestRay
import           TestVec

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, testRay, testVectors]

unitTests =
  testGroup
    "Unit tests"
    [testCase "Gradient Image" $ (calcSize . gradientImg) oriSize @?= oriSize]
  where
    oriSize = Size 8 10
    calcSize arr = Size (length . head $ arr) (length arr)
