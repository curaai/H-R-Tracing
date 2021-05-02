{-# LANGUAGE ParallelListComp #-}

module Camera where

import           Ray
import           Vector

import           Hittable.Hittable
import           Hittable.Sphere
import           System.Random

data Size a =
  Size
    { w :: a
    , h :: a
    }
  deriving (Show, Eq)

data Camera =
  Camera
    { aspectRatio    :: Float
    , imageSize      :: Size Int
    , viewportSize   :: Size Float
    , cPos           :: Vec3 Float
    , cDirection     :: Vec3 Float
    , focalLength    :: Float
    , samplePerPixel :: Int
    }
  deriving (Show)

toFloat x = fromIntegral x :: Float

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
    range' f = map toFloat [0,1 .. f size]

posToRay :: Camera -> (Float, Float) -> Ray
posToRay cam (u, v) = Ray (cPos cam) (llc + hv *: u + vv *: v - cPos cam)
  where
    llc = lowerLeftCorner cam
    hv = camHVec cam
    vv = camVVec cam

hitRay :: Hittable a => a -> Ray -> Either HitRecord Ray
hitRay spheres ray =
  case c of
    Just x  -> Left x
    Nothing -> Right ray
  where
    c = shotRay spheres ray (RootRange 0.0 999.9)

ray2vec :: Either HitRecord Ray -> Vec3 Float
ray2vec (Left v) = (normal v + Vec3 1 1 1) *: 0.5
ray2vec (Right r) = a *: (1.0 - t) + b *: t
  where
    a = Vec3 1.0 1.0 1.0
    b = Vec3 0.5 0.7 1.0
    t = 0.5 * (_y unitDir + 1.0)
    unitDir = vUnit . direction $ r

screen :: Hittable a => Camera -> a -> [[Color]]
screen cam spheres = map (map (vec2color' . sampling)) img'
  where
    sampling (x, y) =
      sum $
      take
        (samplePerPixel cam)
        [ ray2vec . hitRay' . posToRay' $
          ((x + i) / (toFloat . w) size, (y + j) / (toFloat . h) size)
        | i <- rand1
        | j <- rand2
        ]
      where
        size = imageSize cam
        rand1 = randoms (mkStdGen 100) :: [Float]
        rand2 = randoms (mkStdGen 101) :: [Float]
    vec2color' = vec2color $ toFloat . samplePerPixel $ cam
    hitRay' = hitRay spheres
    posToRay' = posToRay cam
    img' = make2d . imageSize $ cam

data Color =
  Color
    { _r :: Int
    , _g :: Int
    , _b :: Int
    }
  deriving (Show, Eq)

vec2color :: RealFrac a => a -> Vec3 a -> Color
vec2color spp v = Color (_x cv) (_y cv) (_z cv)
  where
    cv = fmap (truncate . (* 256) . clamp . (* (1 / spp))) v
    clamp x = max 0 . min 0.999 $ x
