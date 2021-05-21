module Camera where

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

render :: Camera -> [(Float, Float)] -> [Color Float]
render cam = map (ray2color . pos2ray cam)

pos2ray :: Camera -> (Float, Float) -> Ray
pos2ray cam (u, v) =
  let llc = lowerLeftCorner cam
   in Ray
        (cameraPos cam)
        (llc + pure u * camHVec cam + pure v * camVVec cam - cameraPos cam)

ray2color :: Ray -> Vec3 Float
ray2color r =
  let t = hitSphere (Vec3 0 0 (-1)) 0.5 r
   in if 0 < t
        then (* 0.5) . (+ 1) . vUnit $ at r t - Vec3 0 0 (-1)
        else let t' = 0.5 * ((+ 1) . _y . vUnit . direction $ r)
              in pure (1 - t') + pure t' * Vec3 0.5 0.7 1.0
  where
    hitSphere ctr radius ray =
      let discriminant = halfB ^ 2 - a * c
       in if discriminant < 0
            then -1
            else ((-halfB) - sqrt discriminant) / a
      where
        oc = origin ray - ctr
        a = vLengthSquared . direction $ ray
        halfB = vDot oc (direction ray)
        c = vLengthSquared oc - radius ^ 2
