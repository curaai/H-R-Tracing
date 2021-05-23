module Camera where

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
     (Integral b, Hittable a) => Camera -> Size Int -> Int -> a -> [Color b]
render cam size spp objs = map computeColor coords
  where
    coords = (,) <$> reverse [0 .. height size - 1] <*> [0 .. width size - 1]
    computeColor (y, x) =
      vec2color spp . fst $ foldl sampling (Vec3 0 0 0, g) [1 .. spp]
      where
        g = mkStdGen $ y * width size + x
        sampling :: RandomGen g => (Vec3 Float, g) -> a -> (Vec3 Float, g)
        sampling (acc, g) _ = (acc + (ray2color objs . pos2ray cam) (u, v), g')
          where
            func f v = v / ((+ (-1)) . toFloat . f) size
            (u, v) = (func width (toFloat x + i), func height (toFloat y + j))
            (i, g1) = sampleFloat g
            (j, g') = sampleFloat g1

vec2color :: (Integral b, Integral a) => a -> Vec3 Float -> Color b
vec2color spp = fmap (truncate . (* 256) . clamp 0 0.999 . (/ toFloat spp))
  where
    clamp min' max' x = max min' . min max' $ x

pos2ray :: Camera -> (Float, Float) -> Ray
pos2ray cam (u, v) =
  let llc = lowerLeftCorner cam
   in Ray
        (cameraPos cam)
        (llc + pure u * camHVec cam + pure v * camVVec cam - cameraPos cam)

ray2color :: Hittable a => a -> Ray -> Vec3 Float
ray2color objs r =
  let hr = hit objs r (HitRange 0 maxValue)
   in maybe backgroundRayColor ((* 0.5) . (+ 1) . hitNormal) hr
  where
    backgroundRayColor =
      let t' = 0.5 * ((+ 1) . _y . vUnit . direction $ r)
       in pure (1 - t') + pure t' * Vec3 0.5 0.7 1.0
