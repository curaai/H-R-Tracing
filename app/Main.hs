module Main where

import qualified Data.ByteString.Char8 as C

import           Img
import           Vector

import           Camera
import           Hittable.Hittable
import           Hittable.Sphere

main :: IO ()
main = C.writeFile "res.ppm" . C.pack . toPpmStr $ screen camera spheres
  where
    camera =
      Camera
        (16 / 9)
        (Size 400 (truncate $ 400 * 9 / 16))
        (Size (16 / 9 * 2) 2)
        (Vec3 0 0 0)
        (Vec3 0 0 0)
        1
    spheres = [Sphere (Vec3 0 0 (-1)) 0.5, Sphere (Vec3 0 (-100.5) (-1)) 100]
