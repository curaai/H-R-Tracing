module Camera where

import Hitable
import Img
import Ray
import Vector

import Data.Either

data Camera = Camera { aspectRatio :: Float
                     , imageSize :: Size Int
                     , viewportSize :: Size Float
                     , cPos:: Vec3 Float
                     , cDirection :: Vec3 Float
                     , focalLength :: Float
                     } deriving(Show)

camHVec :: Camera -> Vec3 Float
camHVec cam = Vec3 (w . viewportSize $ cam) 0.0 0.0
camVVec :: Camera -> Vec3 Float
camVVec cam = Vec3 0 (h . viewportSize $ cam) 0.0
camLVec :: Camera -> Vec3 Float
camLVec cam = Vec3 0.0 0 (focalLength cam)


lowerLeftCorner cam = cPos cam - camHVec cam *: 0.5 - camVVec cam *: 0.5 - camLVec cam

make2d :: Integral a => Size a -> [[(Float, Float)]]
make2d size = [[(i, j) | i <- range' w ] | j <- reverse . range' $ h]
  where range' f = [0, 1 / ((\x -> fromIntegral x :: Float) . f) size .. 1]

screen cam sphere = map (map (ray2color . checkHit . toRay)) img'
  where
    checkHit r = if isHit sphere r then Left (Vec3 1.0 0 0) else Right r
    ray2color (Left v) = v
    ray2color (Right r) = a *: (1.0 - t) + b *: t
      where 
        a = Vec3 1.0 1.0 1.0
        b = Vec3 0.5 0.7 1.0
        t = 0.5 * (_y unitDir + 1.0)
        unitDir = vUnit . direction $ r
    toRay (u, v) = Ray (cPos cam) (llc + hv *: u + vv *: v - cPos cam)
      where
        llc = lowerLeftCorner cam
        hv = camHVec cam
        vv = camVVec cam
    img' = make2d . imageSize $ cam
