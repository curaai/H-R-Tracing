{-# LANGUAGE ExistentialQuantification #-}

module Camera where

import           Data.Maybe                  (fromJust, isJust, isNothing)

import           Hit                         (HitRange (HitRange),
                                              HitRecord (HitRecord, hitMaterial),
                                              Material (Material),
                                              Scatterable (scatter),
                                              Scattered (Scattered))

import           Hittable.Hittable           (Hittable (..))
import           Numeric.Limits              (maxValue)
import           Ray                         (Ray (Ray, direction))
import           Sampling                    (sampleFloat, sampleInUnitDisk)
import           System.Random               (RandomGen, mkStdGen)
import           Vector                      (Color, Point, Vec,
                                              Vec3 (Vec3, _x, _y), vUnit)

import           Control.Parallel.Strategies (parBuffer, rseq, withStrategy)

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
pos2ray (Camera origin' horizontal' vertical' llc cw cu cv lensRaidus') (u, v) g =
  ( Ray
      (origin' + offset)
      (llc + pure u * horizontal' + pure v * vertical' - origin' - offset)
  , g')
  where
    (randUnitDisk, g') = sampleInUnitDisk g
    rd = pure lensRaidus' * randUnitDisk
    offset = cu * (pure . _x) rd + cv * (pure . _y) rd

toFloat :: Integral a => a -> Float
toFloat x = fromIntegral x :: Float

render :: (Hittable a2) => Camera -> Size Int -> Int -> Int -> a2 -> [Color Int]
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
  fmap (truncate . (* 256) . (max 0 . min 0.999) . sqrt . (/ toFloat spp))

ray2color ::
     (RandomGen t2, Hittable a) => a -> t2 -> Int -> Ray -> (Vec, t2)
ray2color objs g depth r
  | depth <= 0 = (Vec3 0 0 0, g)
  | isNothing hr = (bgRayColor r, g)
  | otherwise = hitRecursively (fromJust hr) g
  where
    hr = hit objs r (HitRange 0.001 maxValue)
    hitRecursively hr@HitRecord {hitMaterial = Material m} g
      | isJust _scattered =
        let (Scattered sctRay sctColor) = fromJust _scattered
            (color, g') = ray2color objs g1 (depth - 1) sctRay
         in (sctColor * color, g')
      | otherwise = (Vec3 0 0 0, g)
      where
        (_scattered, g1) = scatter m r hr g

bgRayColor :: Ray -> Vec3 Float
bgRayColor Ray {direction = dir} =
  let t = 0.5 * ((+ 1) . _y . vUnit $ dir)
   in pure (1 - t) + pure t * Vec3 0.5 0.7 1.0
