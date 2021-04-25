module Main where
import qualified Data.ByteString.Char8 as C

import Vector
import Img

import Hitable
import Camera

main :: IO ()
main = C.writeFile "res.ppm" . C.pack . toPpmStr $ screen camera sphere 
  where 
    camera = Camera (16/9) (Size 400 (truncate $ 400*9/16)) (Size (16/9*2) 2) (Vec3 0 0 0) (Vec3 0 0 0) 1
    sphere = Sphere (Vec3 0 0 (-1.0)) 0.5
