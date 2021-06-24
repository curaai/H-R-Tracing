module Main where

import qualified Data.ByteString.Char8 as C

import           Camera                (Camera (Camera), Size (..), render)
import           Data.Maybe            (catMaybes)
import           Hit                   (Material (Material))
import           Hittable.Hittable     (Hittable)
import           Hittable.HittableList ()
import           Hittable.Sphere       (Sphere (Sphere))
import           Material.Dielectric   (Dielectric (Dielectric))
import           Material.Lambertian   (Lambertian (Lambertian))
import           Material.Metal        (Metal (Metal))
import           Sampling              (sampleFloat, sampleVector,
                                        sampleVectorR)
import           System.Random         (RandomGen, mkStdGen, randomR)
import           Vector                (Point, Vec3 (Vec3), vCross, vLength,
                                        vUnit)

aspectRatio :: Float
aspectRatio = 16 / 9

drawImg :: Hittable a2 => Size Int -> a2 -> String
drawImg size hittables = unlines $ "P3" : size' : "255" : map show arr
  where
    arr = render cam size 20 75 hittables
      where
        lookfrom = Vec3 13 2 3
        lookat = Vec3 0 0 0
        cam = mkCamera lookfrom lookat (Vec3 0 1 0) 20 aspectRatio 0.1 10
    size' = unwords . map show $ [width size, height size]

mkCamera ::
     Point
  -> Vec3 Float
  -> Vec3 Float
  -> Float
  -> Float
  -> Float
  -> Float
  -> Camera
mkCamera lookfrom lookat vup vfov aspectRatio aperture focusDist =
  Camera
    lookfrom
    horizontal'
    vertical'
    (lookfrom - horizontal' / pure 2 - vertical' / pure 2 - pure focusDist * w)
    w
    u
    v
    (aperture / 2)
  where
    degree2radian x = x * pi / 180
    theta = degree2radian vfov
    h = tan $ theta / 2
    viewportHeight = 2 * h
    viewportWidth = aspectRatio * viewportHeight
    w = vUnit $ lookfrom - lookat
    u = vUnit $vCross vup w
    v = vCross w u
    horizontal' = pure (focusDist * viewportWidth) * u
    vertical' = pure (focusDist * viewportHeight) * v

mkRandomScene :: RandomGen b => b -> [Sphere]
mkRandomScene g = [groundObj, obj1, obj2, obj3] ++ objs
  where
    groundObj =
      Sphere (Vec3 0 (-1000) 0) 1000 $ Material (Lambertian (Vec3 0.5 0.5 0.5))
    obj1 = Sphere (Vec3 0 1 0) 1 $ Material (Dielectric 1.5)
    obj2 = Sphere (Vec3 (-4) 1 0) 1 $ Material (Lambertian (Vec3 0.4 0.2 0.1))
    obj3 = Sphere (Vec3 4 1 0) 1 $ Material (Metal (Vec3 0.7 0.6 0.5) 0.0)
    objs =
      catMaybes . fst $
      foldl getRandObj ([], g) $ (,) <$> reverse [-11 .. 11] <*> [-11 .. 11]

getRandObj ::
     RandomGen b => ([Maybe Sphere], b) -> (Float, Float) -> ([Maybe Sphere], b)
getRandObj (objs, g) (a, b) = (o : objs, g3)
  where
    o =
      if vLength (center - Vec3 4 0.2 0) > 0.9
        then Just (Sphere center 0.2 material)
        else Nothing
    mkRandPos g = (Vec3 (a + 0.9 * rx) 0.2 (b + 0.9 * ry), g')
      where
        (rx, g1) = sampleFloat g
        (ry, g') = sampleFloat g1
    mkRandMaterial randMaterialType g
      | randMaterialType < 0.8 =
        let (c1, g1) = sampleVector g
            (c2, g') = sampleVector g1
         in (Material (Lambertian $ c1 * c2), g')
      | randMaterialType < 0.95 =
        let (albedo', g1) = sampleVectorR g 0.5 1
            (fuzz', g2) = randomR (0, 0.5) g1
         in (Material (Metal albedo' fuzz'), g2)
      | otherwise = (Material (Dielectric 1.5), g)
    (randMaterialType, g1) = sampleFloat g
    (center, g2) = mkRandPos g1
    (material, g3) = mkRandMaterial randMaterialType g2

main :: IO ()
main =
  C.writeFile "res.ppm" . C.pack $
  drawImg (Size w (truncate ((fromIntegral w :: Float) / aspectRatio))) world
  where
    w = 1280 :: Int
    world = mkRandomScene $ mkStdGen 1
