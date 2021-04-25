module Main where
import qualified Data.ByteString.Char8 as C

import Img
import Camera
import Vector

camera = Camera (16/9) (Size 400 300) (Size (16/9*2) 2) (Vec3 0 0 0) (Vec3 0 0 0) 1
    

main :: IO ()
main = 
  C.writeFile "res.ppm" $
    C.pack $ toPpmStr . screen $ camera
