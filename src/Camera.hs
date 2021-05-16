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
camHVec cam = Vec3 (width . viewportSize $ cam) 0.0 0.0

camVVec :: Camera -> Vec3 Float
camVVec cam = Vec3 0 (height . viewportSize $ cam) 0.0

camLVec :: Camera -> Vec3 Float
camLVec cam = Vec3 0.0 0 (focalLength cam)

lowerLeftCorner :: Camera -> Vec3 Float
lowerLeftCorner cam =
  cameraPos cam - camHVec cam * 0.5 - camVVec cam * 0.5 - camLVec cam

render :: Camera -> [(Float, Float)] -> [Color Float]
render cam = map (ray2color . pos2ray)
  where
    ray2color r =
      let t = 0.5 * ((+ 1) . _y . vUnit . direction $ r)
       in pure (1 - t) + pure t * Vec3 0.5 0.7 1.0
    pos2ray (u, v) =
      let llc = lowerLeftCorner cam
       in Ray
            (cameraPos cam)
            (llc + pure u * camHVec cam + pure v * camVVec cam - cameraPos cam)

vec2color :: (Floating c, RealFrac c, Integral a) => c -> Vec3 c -> Vec3 a
vec2color spp v = Vec3 (_x cv) (_y cv) (_z cv)
  where
    cv = fmap (truncate . (* 256) . clamp . gammaCorrection) v
    clamp x = max 0 . min 0.999 $ x
    gammaCorrection = sqrt . (* (1 / spp))
