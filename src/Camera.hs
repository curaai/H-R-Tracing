module Camera where

import           Data.Maybe
import           Hittable.Hittable
import           Numeric.Limits
import           Ray
import           Sampling
import           System.Random
import           Vector

data Size a =
  Size
    { width  :: a
    , height :: a
    }
  deriving (Show, Eq)

data Camera =
  Camera
    { viewportSize :: Size Float
    , cameraPos    :: Vec3 Float
    , cameraDir    :: Vec3 Float
    , focalLength  :: Float
    }
  deriving (Show)

camHVec :: Camera -> Vec3 Float
camHVec cam = Vec3 (width . viewportSize $ cam) 0 0

camVVec :: Camera -> Vec3 Float
camVVec cam = Vec3 0 (height . viewportSize $ cam) 0

camLVec :: Camera -> Vec3 Float
camLVec cam = Vec3 0 0 (focalLength cam)

lowerLeftCorner :: Camera -> Vec3 Float
lowerLeftCorner cam =
  cameraPos cam - camHVec cam * 0.5 - camVVec cam * 0.5 - camLVec cam

toFloat x = fromIntegral x :: Float

render ::
     (Integral b, Hittable a)
  => Camera
  -> Size Int
  -> Int
  -> Int
  -> a
  -> [Color b]
render cam (Size w h) spp rayDepth objs = map computeColor coords
  where
    coords = (,) <$> reverse [0 .. h - 1] <*> [0 .. w - 1]
    computeColor (y, x) =
      vec2color spp . fst $ foldl sampling (Vec3 0 0 0, g) [1 .. spp]
      where
        g = mkStdGen $ y * w + x
        sampling :: RandomGen g => (Vec3 Float, g) -> a -> (Vec3 Float, g)
        sampling (acc, g) _ = (acc + color, g')
          where
            func a b = a / ((+ (-1)) . toFloat) b
            (i, g1) = sampleFloat g
            (j, g2) = sampleFloat g1
            (u, v) = (func (toFloat x + i) w, func (toFloat y + j) h)
            ray = pos2ray cam (u, v)
            (color, g') = ray2color objs g2 rayDepth ray

vec2color :: (Integral b, Integral a) => a -> Vec3 Float -> Color b
vec2color spp =
  fmap (truncate . (* 256) . clamp 0 0.999 . sqrt . (/ toFloat spp))
  where
    clamp min' max' x = max min' . min max' $ x

pos2ray :: Camera -> (Float, Float) -> Ray
pos2ray cam (u, v) =
  let llc = lowerLeftCorner cam
   in Ray
        (cameraPos cam)
        (llc + pure u * camHVec cam + pure v * camVVec cam - cameraPos cam)

ray2color ::
     (RandomGen g, Hittable a) => a -> g -> Int -> Ray -> (Vec3 Float, g)
ray2color objs g depth r
  | depth <= 0 = (Vec3 0 0 0, g)
  | isNothing hr = (backgroundRayColor, g)
  | otherwise = hitRecursively (fromJust hr) g
  where
    hr = hit objs r (HitRange 0.001 maxValue)
    backgroundRayColor =
      let t' = 0.5 * ((+ 1) . _y . vUnit . direction $ r)
       in pure (1 - t') + pure t' * Vec3 0.5 0.7 1.0
    hitRecursively hr g = (0.5 * color, g')
      where
        (randHemisphereVec, g1) = sampleInHemisPhere (hitNormal hr) g
        target = hitPoint hr + randHemisphereVec
        (color, g') =
          ray2color
            objs
            g'
            (depth - 1)
            (Ray (hitPoint hr) (target - hitPoint hr))
