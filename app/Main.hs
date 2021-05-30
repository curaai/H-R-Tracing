module Main where

import qualified Data.ByteString.Char8 as C

import           Camera
import           Hit
import           Hittable.Hittable
import           Hittable.HittableList
import           Hittable.Sphere
import           Material.Dielectric
import           Material.Lambertian
import           Material.Metal
import           Vector

aspectRatio = 16 / 9

drawImg :: Hittable a2 => Size Int -> a2 -> String
drawImg size hittables = unlines $ "P3" : size' : "255" : map show arr
  where
    arr = render cam size 50 50 hittables
      where
        cam =
          mkCamera (Vec3 (-2) 2 1) (Vec3 0 0 (-1)) (Vec3 0 1 0) 20 aspectRatio
    size' = unwords . map show $ [width size, height size]

mkCamera :: Point -> Point -> Vec3 Float -> Float -> Float -> Camera
mkCamera lookfrom lookat vup vfov aspectRatio =
  Camera
    lookfrom
    horizontal'
    vertical'
    (lookfrom - horizontal' / pure 2 - vertical' / pure 2 - w)
  where
    degree2radian x = x * pi / 180
    theta = degree2radian vfov
    h = tan $ theta / 2
    viewportHeight = 2 * h
    viewportWidth = aspectRatio * viewportHeight
    w = vUnit $ lookfrom - lookat
    u = vUnit $vCross vup w
    v = vCross w u
    horizontal' = pure viewportWidth * u
    vertical' = pure viewportHeight * v

main :: IO ()
main =
  C.writeFile "res.ppm" . C.pack $
  drawImg (Size 400 (truncate (400 / aspectRatio))) spheres
  where
    -- r = cos $ pi / 4
    spheres =
      [ Sphere (Vec3 0 (-100.5) (-1)) 100 materialGround
      , Sphere (Vec3 0 0 (-1)) 0.5 materialCenter
      , Sphere (Vec3 (-1) 0 (-1)) 0.5 materialLeft
      , Sphere (Vec3 (-1) 0 (-1)) (-0.45) materialLeft
      , Sphere (Vec3 1 0 (-1)) 0.5 materialRight
      ]
    materialGround = Material (Lambertian (Vec3 0.8 0.8 0))
    materialCenter = Material (Lambertian (Vec3 0.1 0.2 0.5))
    materialLeft = Material (Dielectric 1.5)
    materialRight = Material (Metal (Vec3 0.8 0.6 0.2) 0)
