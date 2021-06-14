{-# LANGUAGE ExistentialQuantification #-}

module Camera where

import           Data.Maybe
import           Hit
import           Hittable.Hittable
import           Hittable.Sphere
import           Material.Dielectric
import           Material.Lambertian
import           Material.Metal
import           Numeric.Limits
import           Ray
import           Sampling
import           System.Random
import           Vector

import           Control.Parallel.Strategies

data Size a =
  Size
    { width  :: a
    , height :: a
    }
  deriving (Show, Eq)

data Camera =
  Camera
    { lookFrom        :: !Point
    , horizontal      :: !Vec
    , vertical        :: !Vec
    , lowerLeftCorner :: !Point
    , camW            :: !Vec
    , camU            :: !Vec
    , camV            :: !Vec
    , lensRaidus      :: !Float
    }
  deriving (Show)

pos2ray :: RandomGen g => Camera -> (Float, Float) -> g -> (Ray, g)
pos2ray cam@(Camera origin' horizontal' vertical' llc cw cu cv lensRaidus') (u, v) g =
  ( Ray
      (origin' + offset)
      (llc + pure u * horizontal' + pure v * vertical' - origin' - offset)
  , g')
  where
    (randUnitDisk, g') = sampleInUnitDisk g
    rd = pure lensRaidus' * randUnitDisk
    offset = cu * (pure . _x) rd + cv * (pure . _y) rd

toFloat x = fromIntegral x :: Float

render cam (Size w h) spp rayDepth objs =
  withStrategy (parBuffer 100 rseq) $ map computeColor coords
  where
    coords = (,) <$> reverse [0 .. h - 1] <*> [0 .. w - 1]
    computeColor (y, x) =
      vec2color spp . fst $ foldl sampling (Vec3 0 0 0, g) [1 .. spp]
      where
        g = mkStdGen $ y * w + x
        sampling :: RandomGen g => (Vec, g) -> a -> (Vec, g)
        sampling (acc, g) _ = (acc + color, g')
          where
            func a b = a / ((+ (-1)) . toFloat) b
            (i, g1) = sampleFloat g
            (j, g2) = sampleFloat g1
            (u, v) = (func (toFloat x + i) w, func (toFloat y + j) h)
            (ray, g3) = pos2ray cam (u, v) g2
            (color, g') = ray2color objs g3 rayDepth ray

vec2color :: (Integral b, Integral a) => a -> Vec -> Color b
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
  -> (Vec, p)
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
