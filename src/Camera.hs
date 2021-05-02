module Camera where

import           Img
import           Ray
import           Vector

import           Hittable.Hittable
import           Hittable.Sphere

data Camera =
  Camera
    { aspectRatio  :: Float
    , imageSize    :: Size Int
    , viewportSize :: Size Float
    , cPos         :: Vec3 Float
    , cDirection   :: Vec3 Float
    , focalLength  :: Float
    }
  deriving (Show)

camHVec :: Camera -> Vec3 Float
camHVec cam = Vec3 (w . viewportSize $ cam) 0.0 0.0

camVVec :: Camera -> Vec3 Float
camVVec cam = Vec3 0 (h . viewportSize $ cam) 0.0

camLVec :: Camera -> Vec3 Float
camLVec cam = Vec3 0.0 0 (focalLength cam)

lowerLeftCorner :: Camera -> Vec3 Float
lowerLeftCorner cam =
  cPos cam - camHVec cam *: 0.5 - camVVec cam *: 0.5 - camLVec cam

make2d :: Integral a => Size a -> [[(Float, Float)]]
make2d size = [[(i, j) | i <- range' w] | j <- reverse . range' $ h]
  where
    range' f = [0,1 / ((\x -> fromIntegral x :: Float) . f) size .. 1]

toRay :: Camera -> (Float, Float) -> Ray
toRay cam (u, v) = Ray (cPos cam) (llc + hv *: u + vv *: v - cPos cam)
  where
    llc = lowerLeftCorner cam
    hv = camHVec cam
    vv = camVVec cam

hitColor :: Hittable a => a -> Ray -> Either HitRecord Ray
hitColor spheres ray =
  case c of
    Just x  -> Left x
    Nothing -> Right ray
  where
    c =
      shotRay
        spheres
        ray
        (RootRange 0.0 999.9)

ray2color :: Either HitRecord Ray -> Vec3 Float
ray2color (Left v) = (normal v + Vec3 1 1 1) *: 0.5
ray2color (Right r) = a *: (1.0 - t) + b *: t
  where
    a = Vec3 1.0 1.0 1.0
    b = Vec3 0.5 0.7 1.0
    t = 0.5 * (_y unitDir + 1.0)
    unitDir = vUnit . direction $ r

screen :: Hittable a => Camera -> a -> [[Vec3 Float]]
screen cam spheres = map (map (ray2color . hitColor' . toRay')) img'
  where
    hitColor' = hitColor spheres
    toRay' = toRay cam
    img' = make2d . imageSize $ cam
