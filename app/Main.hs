module Main where

import qualified Data.ByteString.Char8 as C

import           Camera
import           Hittable.Hittable
import           Hittable.HittableList
import           Hittable.Sphere
import           Vector

aspectRatio = 16 / 9

drawImg size hittables = unlines $ "P3" : size' : "255" : map show arr
  where
    arr = render cam size 50 hittables
      where
        cam =
          Camera
            (Size (aspectRatio * viewportHeight) viewportHeight)
            (pure 0)
            (Vec3 0 0 focalLength')
            focalLength'
          where
            viewportHeight = 2
            focalLength' = 1
    size' = unwords . map show $ [width size, height size]

main :: IO ()
main =
  C.writeFile "res.ppm" . C.pack $
  drawImg (Size 400 (truncate (400 / aspectRatio))) spheres
  where
    spheres = [Sphere (Vec3 0 0 (-1)) 0.5, Sphere (Vec3 0 (-100.5) (-1)) 100]
