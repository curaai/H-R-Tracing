module TestCamera where

import           Camera
import           Data.Maybe            (isJust)
import           Hittable.Sphere
import           Img
import           Ray
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC
import           Vector

testCamera = testGroup "Test Camera" [t0]

cam =
  Camera
    (16 / 9)
    (Size 400 (truncate $ 400 * 9 / 16))
    (Size (16 / 9 * 2) 2)
    (Vec3 0 0 0)
    (Vec3 0 0 0)
    1

t0 =
  testGroup
    "Make Ray test"
    [ testCase "case1" $ toRay' (0, 0) @?= Ray cPos' llc
    , testCase "case2" $ toRay' (0.5, 0.5) @?= Ray cPos' (Vec3 0 0 (-1))
    , testCase "case3" $
      toRay' (1, 1) @?=
      Ray cPos' (Vec3 (negate . _x $ llc) (negate . _y $ llc) (-1))
    ]
  where
    toRay' = toRay cam
    cPos' = cPos cam
    llc = lowerLeftCorner cam

