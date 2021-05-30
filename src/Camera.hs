{-# LANGUAGE ExistentialQuantification #-}

module Camera where

import           Data.Maybe
import           Hit
import           Hittable.Hittable
import           Material.Lambertian
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
    { lookFrom        :: Point
    , horizontal      :: Vec3 Float
    , vertical        :: Vec3 Float
    , lowerLeftCorner :: Point
    }
  deriving (Show)

pos2ray :: Camera -> (Float, Float) -> Ray
pos2ray (Camera origin' horizontal' vertical' llc) (u, v) =
  Ray origin' (llc + pure u * horizontal' + pure v * vertical' - origin')

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

ray2color ::
     (Ord t, RandomGen p, Num t, Hittable a)
  => a
  -> p
  -> t
  -> Ray
  -> (Vec3 Float, p)
ray2color objs g depth r
  | depth <= 0 = (Vec3 0 0 0, g)
  | isNothing hr = (backgroundRayColor, g)
  | otherwise = hitRecursively (fromJust hr) g
  where
    hr = hit objs r (HitRange 0.001 maxValue)
    backgroundRayColor =
      let t' = 0.5 * ((+ 1) . _y . vUnit . direction $ r)
       in pure (1 - t') + pure t' * Vec3 0.5 0.7 1.0
    hitRecursively hr@(HitRecord _ _ _ _ (Material m)) g
      | isJust _scattered =
        let scattered' = fromJust _scattered
            attenuation = attenuationColor scattered'
            (color, g') =
              ray2color objs g1 (depth - 1) (scatteredRay scattered')
         in (attenuation * color, g')
      | otherwise = (Vec3 0 0 0, g)
      where
        (_scattered, g1) = scatter m r hr g
