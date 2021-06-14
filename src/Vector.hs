module Vector where

import           Control.Applicative (Applicative (liftA2))

data Vec3 a =
  Vec3
    { _x :: !a
    , _y :: !a
    , _z :: !a
    }
  deriving (Eq)

type Vec = Vec3 Float

type Point = Vec3 Float

type Color = Vec3

instance Show a => Show (Vec3 a) where
  show = foldMap ((++ " ") . show)

instance Functor Vec3 where
  fmap f (Vec3 a b c) = Vec3 (f a) (f b) (f c)

instance Foldable Vec3 where
  foldMap f (Vec3 a b c) = f a `mappend` f b `mappend` f c

instance Applicative Vec3 where
  pure a = Vec3 a a a
  Vec3 a b c <*> Vec3 d e f = Vec3 (a d) (b e) (c f)

instance Num a => Num (Vec3 a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  negate = fmap negate
  fromInteger = pure . fromInteger

instance Fractional a => Fractional (Vec3 a) where
  (/) = liftA2 (/)
  recip = fmap recip
  fromRational = pure . fromRational

vDot :: Vec -> Vec -> Float
vDot v1 v2 = sum $ v1 * v2

vLength :: Vec -> Float
vLength v = sqrt $ vDot v v

vLengthSquared :: Vec -> Float
vLengthSquared v = vDot v v

vUnit :: Vec -> Vec
vUnit v =
  let k = recip . vLength $ v
   in fmap (* k) v

vCross :: Vec -> Vec -> Vec
vCross v1 v2 = Vec3 x y z
  where
    x = (_y v1 * _z v2) - (_z v1 * _y v2)
    y = (_z v1 * _x v2) - (_x v1 * _z v2)
    z = (_x v1 * _y v2) - (_y v1 * _x v2)

vNearZero :: Vec -> Bool
vNearZero = and . fmap (< 1e-8)

vReflect :: Vec -> Vec -> Vec
vReflect v n = v - pure (2 * vDot v n) * n

vRefract :: Vec -> Vec -> Float -> Vec
vRefract uv n etaiOverEtat = outPerp + outParallel
  where
    cosTheta = min (vDot (-uv) n) 1
    outPerp = pure etaiOverEtat * (uv + pure cosTheta * n)
    outParallel =
      (* n) . pure . negate . sqrt . abs $ 1 - vLengthSquared outPerp
