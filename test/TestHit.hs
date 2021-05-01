module TestHit where

import           Data.Maybe            (isJust)
import           Hittable.Sphere
import           Ray
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC
import           Vector

testSphere = testGroup "Test Hit to sphere" [t0]

t0 = testCase "Is Hitted" $ isJust (hitNormal s r) @?= True
  where
    r = Ray (Vec3 0 0 0) (Vec3 0 0 1)
    s = Sphere (Vec3 0 0 0) 0.5
-- t0 = testCase "Is Hitted" $ hit s r >= 0 @?= True
--   where
--     r = Ray (Vec3 0 0 0) (Vec3 0 0 1)
--     s = Sphere (Vec3 0 0 0) 0.5
