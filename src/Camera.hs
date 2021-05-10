module Camera where

import           Ray
import           Sampling
import           Vector

import           Hittable.Hittable
import           Hittable.Sphere
import           System.Random

data Size a =
  Size
    { width  :: a
    , height :: a
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
camHVec cam = Vec3 (width . viewportSize $ cam) 0.0 0.0

camVVec :: Camera -> Vec3 Float
camVVec cam = Vec3 0 (height . viewportSize $ cam) 0.0

camLVec :: Camera -> Vec3 Float
camLVec cam = Vec3 0.0 0 (focalLength cam)

lowerLeftCorner :: Camera -> Vec3 Float
lowerLeftCorner cam =
  cPos cam - camHVec cam *: 0.5 - camVVec cam *: 0.5 - camLVec cam

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

rayColor ::
     (Ord t, Hittable a, RandomGen b, Num t)
  => a
  -> Ray
  -> t
  -> b
  -> (Vec3 Float, b)
rayColor spheres ray cnt g
  | cnt <= 0 = (Vec3 0 0 0, g)
  | otherwise = ray2vec $ hitRay spheres ray
  where
    ray2vec (Left record) = (fst res *: 0.5, snd res)
      where
        (rus, g') = sampleUnitSphere g
        target = point record + normal record + rus
        res =
          rayColor
            spheres
            (Ray (point record) (target - point record))
            (cnt - 1)
            g'
    ray2vec (Right r) = (a *: (1.0 - t) + b *: t, g)
      where
        a = Vec3 1.0 1.0 1.0
        b = Vec3 0.5 0.7 1.0
        t = 0.5 * (_y unitDir + 1.0)
        unitDir = vUnit . direction $ r

data Color =
  Color
    { _r :: Int
    , _g :: Int
    , _b :: Int
    }
  deriving (Show, Eq)

vec2color spp v = Color (_x cv) (_y cv) (_z cv)
  where
    cv = fmap (truncate . (* 256) . clamp . sqrt . (* (1 / spp))) v
    clamp x = max 0 . min 0.999 $ x
