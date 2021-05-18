import           Data.List
import           Data.Ord

import           Test.Tasty
import           Test.Tasty.HUnit

import           TestCam
import           TestVec

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [testVectors, testRender]
