module Vector where

import Control.Applicative

data Vec3 a = Vec3 {_x :: a, _y :: a, _z ::a} deriving (Show)

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
  fromInteger = pure . fromInteger
  negate = fmap negate

type Vec = Vec3 Float 
type Point = Vec3 Float 

vDot :: Num a => Vec3 a -> Vec3 a -> a
vDot v1 v2 = sum $ v1 * v2

vLength :: Floating a => Vec3 a -> a
vLength v = sqrt $ vDot v v 

vUnit :: Floating b => Vec3 b -> Vec3 b
vUnit v = fmap (* k) v
  where k = 1 / vLength v

vCross :: Num a => Vec3 a -> Vec3 a -> Vec3 a
vCross v1 v2 = Vec3 x y z 
  where 
    x = (_y v1 * _z v2) - (_z v1 * _y v2)
    y = negate $ (_x v1 * _z v2) - (_z v1 * _x v2)
    z = (_x v1 * _y v2) - (_y v1 * _x v2)

v *: x = fmap (*x) v 