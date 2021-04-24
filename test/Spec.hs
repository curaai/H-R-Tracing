import Data.List
import Data.Ord
import Test.Tasty
import Test.Tasty.HUnit

import Img

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests =
  testGroup
    "Unit tests"
    [ testCase "Gradient Image" $
        (calcSize . gradientImg) oriSize @?= oriSize
    ]
    where 
      oriSize = Size 8 10
      calcSize arr = Size (length . head $ arr) (length arr)
        
