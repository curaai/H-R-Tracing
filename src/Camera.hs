module Camera where

import           Data.Maybe
import           Hittable.Hittable
import           Hittable.Sphere
import           Numeric.Limits
import           Ray
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

render :: Hittable a => Camera -> a -> [(Float, Float)] -> [Vec3 Float]
render cam objs = map (ray2color objs . pos2ray cam)

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
