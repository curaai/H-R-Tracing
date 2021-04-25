{-# LANGUAGE TupleSections #-}

module Img where


import Data.List
import Vector

data Color = Color {_r :: Int, _g :: Int, _b :: Int} deriving (Show, Eq)

data Size a = Size {w :: a, h :: a} deriving(Show, Eq)

aspectRatio s = (toFloat . w) s / (toFloat . h) s 
  where toFloat = \x -> fromIntegral x :: Float

vec2color v = Color (_x cv) (_y cv) (_z cv)
  where
    cv = fmap (truncate . (* 255.99)) v

gradientImg size = map (map pair2vec) img
  where
    pair2vec p = uncurry Vec3 p blue
    img = map (uncurry makePair) rows
    makePair y = map (y,)
    blue = 0.15
    rows = zip fYRange $ replicate (h size) fXRange :: [(Float, [Float])]
    fXRange = fRange . w $ size
    fYRange = fRange . h $ size
    fRange n = init [0, 1/(fromIntegral n :: Float) .. 1]

toPpmStr arr = header ++ concatMap (concatMap (color2str . vec2color)) arr
  where
    header = unwords ["P3", show . w $ size, show . h $ size, "255\n"]
    size = Size (length . head $ arr) (length arr)
    color2str c = unwords [show . _r $ c, show . _g $ c, (show . _b $ c) ++ "\n"]
